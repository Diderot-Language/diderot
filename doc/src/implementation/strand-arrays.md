## Strand arrays

Strand arrays provide a standard interface to the underlying representation of strand
status and state information.  There are four different implementations of the
`strand_array` struct type (for the sequential target) and the compiler picks the one
best suited to the particular Diderot program being compiled.

### Strand array operations

There are several different functions for querying the number of strands:
```cpp
// return the number of active strands (includes stabilizing and dying strands)
  uint32_t num_active () const;
// return the current number of active strands (not including stabilizing and dying strands)
  uint32_t current_active () const;
// return the number of stable strands
  uint32_t num_stable () const;
// return the number of alive strands (active + stable)
  uint32_t num_alive () const;
```
All of these functions, except `current_active` return the number of strands as of the
*beginning* of the current superstep.  The `current_active` function returns the number
of currently active strands after accounting for any strands that have stabilized or
died.  The `current_active` function is used to implement the `kill_all` and
`stabilize_all` functions in the `world` struct.

There are two ways to address a strand: a strand's ID (`sid_t`), which is invariant
over the life of a strand, and a strand's index (`index_t`), which may change between
supersteps.  Strand indices are used to provide a fast iteration over strands by status
(*i.e.*, stable, active, or alive).  The `strands_array` struct provides various methods
to access the strand status and state:
```cpp
// return the strand ID for the given strand index
  sid_t id (index_t ix) const;
// return a pointer to the strand with the given strand ID
  STRAND_strand *id_to_strand sid_t id) const;
// return the status of the strand with the given index
  diderot::strand_status status (index_t ix) const;
// return the status of the strand with the given index
  STRAND_strand *strand (index_t ix) const;
// return a pointer to the local (non-shared) state of the strand with the
// given index
  STRAND_local *local_state (index_t ix) const;
// return a pointer to the local (non-shared) state of the strand with the
// given strand ID
  STRAND_local *id_to_local_state (sid_t id) const;
```

#### Iterators

```cpp
// iteration over the active strands in the array
  index_t begin_active () const;
  index_t end_active () const;
  index_t next_active (index_t &ix);
```

```cpp
// iteration over the stable strands in the array
  index_t begin_stable () const;
  index_t end_stable () const;
  index_t next_stable (index_t &ix);
```

```cpp
// iteration over the alive strands in the array
  index_t begin_alive () const;
  index_t end_alive () const;
  index_t next_alive (index_t &ix);
```

#### Dual-state functions

Some functions are specific to the *dual-state* implementations of the `strand_array` type.

The dual-state implementations maintain two copies of the shared mutable strand state:
the *in-state*, which is the strand's state at the beginning of the super step, and
the *out-state*, which is the state that is updated during the super step.

There are two functions for managing the two copies of the mutable state.
```cpp
// for dual-state implementations, return the index of the shared input state.
  uint32_t in_state_index () const;
// swap in and out states
  void swap ();
```
These functions are present, but no-ops, in the single-state implementations.

There are also functions for accessing the in and out states of a strand:
```cpp
// return a pointer to the shared input state of the strand with the
// given index [DUAL-STATE only]
  const STRAND_shared *in_state (index_t ix) const;
// return a pointer to the shared input state of the strand with the
// given strand ID
  const STRAND_shared *id_to_in_state (sid_t id) const;
// return a pointer to the shared output state of the strand with the
// given index
  STRAND_shared *out_state (index_t ix) const;
```

#### Strand operations

```cpp
// initialize the first nStrands locations as new active strands
  void create_strands (uint32_t nStrands);
// invoke strand's start method
  diderot::strand_status strand_start (..., index_t ix);
// invoke strand's update method
  diderot::strand_status strand_update (..., index_t ix);
// invoke strand's stabilize method
  index_t strand_stabilize (..., index_t ix);
// record that the specified strand is dying
  index_t kill (index_t ix);
// allocate a new strand
  index_t new_strand ();
// finish a step by updating the strand statuses and the various counters
  void finish_step ();
```

#### Other operations

```cpp
// allocate space for at least nItems
  bool alloc (uint32_t nItems);
// deallocate space reserved for strands
  void dealloc ();
```

### Implementations

There are four implementations of the `strand_array` type for the sequential target.
These are defined in four different source-fragment files, which are located in the
directory `src/compiler/target-cpu/fragments`.

  * `seq-sarr.in` -- the basic implementation for the no-BSP scheduler with direct
      access to the state and no shared state.  In this implementation a strand's `sid_t`
      and `index_t` IDs are the same.
  * `seq-sarr-indirect.in` -- the implementation for programs that have dynamic thread
      operations (*e.g.*, ``die``).  This implementation adds a level of indirection
      to the strand's state to support the dynamically changing number of strands.
  * `seq-sarr-dual.in` -- the implementation for programs that have strand communication,
       but not `new` or `die`.  Two copies of any mutable strand state that is accessed
       by other strands is duplicated into an *in-state* and *out-state*.
  * `seq-sarr-dual-indirect.in` -- the implementation for programs that have both
       strand communication and dynamically varying numbers of strands.

