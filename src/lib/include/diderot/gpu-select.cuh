#ifndef _DIDEROT_GPU_SELECT_
#define _DIDEROT_GPU_SELECT_

#include "base.h"
#include "options.h"
#include <cmath>

namespace diderot {
class gpu_selector {
private:
    bool interactive = false;
    int32_t gpuIdx = -1;

    bool isValidGpu(cudaDeviceProp &deviceProperties) {
        return deviceProperties.major >= 6 && deviceProperties.totalGlobalMem >= 1000 * 1000 * 1000 /*1GB*/;
    }

public:
    void registerOptions(diderot::options *options) {
        options->addFlag("ig,interactive-gpu", "Interactively select CUDA GPU when launching", &interactive);
        options->add("gpu", "Specify id of CUDA GPU to select", &gpuIdx, true);
    }

    int32_t* get_gpuIdx() {
        return &gpuIdx;
    }

    void selectGpu() {
        if (gpuIdx < -1) {
            std::cerr << "Warning: GPU id cannot be negative - ignoring" << std::endl;
        }
        bool validGpuId = gpuIdx >= 0;
        if (interactive && validGpuId) {
            std::cerr << "Warning: GPU specified, ignoring interactive GPU selection flag" << std::endl;
        }
        int devicesCount;
        cudaGetDeviceCount(&devicesCount);
        if (devicesCount == 0) {
            std::cerr << "Error: No CUDA GPUs found." << std::endl;
            exit(1);
        }
        int defaultGpuIdx;
        cudaGetDevice(&defaultGpuIdx);
        if (!validGpuId && interactive) {
            std::cout << "Available GPUs:" << std::endl;
            std::cout << "| id | name | memory | max grid | max threads per block |\n";
            for (int i = 0; i < devicesCount; i++) {
                cudaDeviceProp deviceProperties{};
                cudaGetDeviceProperties(&deviceProperties, i);
                if (!isValidGpu(deviceProperties)) {
                    continue;
                }
                int maxGridSize = std::min(deviceProperties.maxGridSize[0],
                                           std::min(deviceProperties.maxGridSize[1], deviceProperties.maxGridSize[2]));
                printf("| %d | %s | %zu | %d | %d |\n", i, deviceProperties.name, deviceProperties.totalGlobalMem,
                       maxGridSize, deviceProperties.maxThreadsPerBlock);
            }
            std::cout << std::endl;
            do {
                std::cout << "Enter GPU id:" << std::endl;
                std::string input;
                getline(std::cin, input);
                try {
                    gpuIdx = std::stoi(input);
                } catch (std::invalid_argument) {
                }
            } while (gpuIdx < 0 || gpuIdx >= devicesCount);
        }
        validGpuId = gpuIdx >= 0 && gpuIdx <= devicesCount;
        cudaError_t gpuSelectionErr = cudaSuccess;
        if (validGpuId) {
            cudaDeviceProp deviceProperties{};
            cudaGetDeviceProperties(&deviceProperties, gpuIdx);
            if (!isValidGpu(deviceProperties)) {
                std::cerr
                        << "Warning: selected invalid GPU. Falling back to default GPU. Please use interactive GPU selection if unsure";
                return;
            }
            std::cout << "Set GPU to: " << deviceProperties.name << " (id:" << gpuIdx << ")" << std::endl;
            gpuSelectionErr = cudaSetDevice(gpuIdx);
            if (gpuSelectionErr == cudaErrorDeviceAlreadyInUse) {
                std::cerr << "Warning: GPU already in use - falling back to default" << std::endl;
            } else if (gpuSelectionErr == cudaErrorInvalidDevice) { // Should technically not happen
                std::cerr << "Warning: GPU invalid - falling back to default" << std::endl;
            }
        }
        if ((!validGpuId || gpuSelectionErr != cudaSuccess) &&
            devicesCount > 1) { // No need to display selection when single GPU
            cudaDeviceProp deviceProperties{};
            cudaGetDeviceProperties(&deviceProperties, defaultGpuIdx);
            std::cout << "Using default GPU: " << deviceProperties.name << " (id:" << defaultGpuIdx << ")" << std::endl;
            gpuSelectionErr = cudaSetDevice(defaultGpuIdx);
            if (gpuSelectionErr == cudaErrorDeviceAlreadyInUse) {
                std::cerr << "Error: Default/Fallback GPU already in use" << std::endl;
                exit(1);
            } else if (gpuSelectionErr == cudaErrorInvalidDevice) { // Should technically not happen
                std::cerr << "Warning: Default/Fallback GPU invalid" << std::endl;
                exit(1);
            }
        }
    }
};
}
#endif //_DIDEROT_GPU_SELECT_
