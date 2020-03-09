# Mesher

## Presentation
A mesher based on the QGis API

The aim of this project is to provide a Mesher in a GIS environment based on QGIS API.

This mesher will be able to generate TIN DEM and other unstructured grids for hydraulic models.

It will be a part of the Reos project, a free and open sources solution dedicated to hydrology and surface hydraulic (http://www.project.reos.site).

## TODO list
- [x] add flip faces functionality
- [x] Save/open TIN using UGRID format
- [x] add possibility to modify the Z value of vertices.
- [x] add tool to set automaticaly the Z value for the vertices.
- [x] Import points and lines from vector layers.
- [ ] Move vertices.
- [x] Open a QGIS 3D map view and display the TIN when editing it in the 2D map view
- [ ] Snap on vertices and layers

![](mesher.gif)

![](mesher_2.gif)

## Working principle with QGIS API
![](TIN_Editor_principle.PNG)
