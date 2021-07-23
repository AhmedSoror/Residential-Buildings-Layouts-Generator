# Residential-Buildings-Layouts-Generator

Spatial Layout is a problem encountered in many real world domains. Such domain is building floor plans
in Architecture, which is usually done manually, where an architect aims typically to divide a specific
area into different high level components, and try to find suitable placements for some sort of low level
modules inside these high level components.
In the frame of a residential building, these high level components could correspond to apartments,
corridor, stairs and elevator rooms, and may also include units like ducts (next to modules like kitchens
or bathrooms), while the low level modules include typically different room types such as bedrooms,
kitchens, bathrooms, living room, sun-room, dressing room, hallways or corridors inside an apartment,
etc.

A residential building layout plan includes one obvious restriction, that is no two different units (ex.
rooms) can be placed on the same position on the plan; moreover, several other constraints could be in
the mind of the architect, these may include that the all room must have a source of day-lighting, kitchens
and bathrooms must be on the side of at least one duct, that the plan shall contain a symmetry of some
kind, and lots of other techniques trying to utilize spaces, raise the price of the units, and aiming to offer
a better resident experience.

Different techniques of Operations Research and AI could be used to solve such problem in automated
way; Throughout this project you will attempt to apply the paradigm of constraint programming.

Sample queries could be found in `queries.pl`
