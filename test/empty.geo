// -----------------------------------------------------------------------------
// How to run
// gmsh -parse_and_exit empty.geo
// -----------------------------------------------------------------------------
Mesh.SaveAll = 1;

Mesh.Binary = 0;
Save "empty-ASCII.msh2";
Save "empty-ASCII.msh4";
