## Life of a strand

This note documents the various stages in the life of a strand and, specifically,
when they occur with respect to the super-steps.

The important events in the life of a strand are:

1. allocation

2. initialization -- the strand initialization method is invoked when the strand
   is created; either by a ``create_grid`` or ``create_collection`` operation
   at the start of the program, or by a ``new`` operation.  
   A strand created by ``new`` is visible to the program starting with the global
   update phase of the superstep in which it was created.

3. start method (optional) --
   the start method is run during the strand-update phase prior to the strand's
   first update.  Any state changes caused by the start method are **not** visible
   to other strands until the next super-step.

4. updates --
   the strand update method is invoked each super-step as long as the strand is
   active and the program is running.

5. termination
    * stabilize
    * die
    * global stabilize
    * global die
