Tracer
======

This is a minimalistic definition of a contravariant functor called `Tracer`
intended to express the pattern common to logging, tracing, monitoring, etc.
in which domain-specific values are provided to domain-agnostic processors.
The `Contravariant` instance on `Tracer m` is the mechanism by which these
general-purpose tracers are adapted to stand in where application-specific
tracers are required.

Find documentation and examples in [the source file](src/Control/Tracer.hs).
