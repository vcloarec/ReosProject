/***************************************************************************
                      reosmesh.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email          : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmesh.h"

ReosMesh::~ReosMesh() {}

VertexPointer ReosMesh::vertex( double x, double y ) const
{
  return vertex( x, y, mTolerance );
}

int ReosMesh::writeUGRIDFormat( std::string fileName )
{
  int ncId;
  int error;

  if ( fileName.empty() )
    return 1;

  error = nc_create( fileName.c_str(), NC_CLOBBER | NC_NETCDF4, &ncId );
  if ( error != NC_NOERR )
    return error;

  std::unique_ptr<MeshIO> reader = getReader();

  std::string meshNodeXVariableName( "TIN_node_x" );
  std::string meshNodeYVariableName( "TIN_node_y" );
  std::string meshNodeZVariableName( "TIN_node_z" );
  std::string meshFaceNodeConnectivity( "TIN_face_node_connectivity" );
  std::string coordinateSystemVariableName( "Coordinate system" );

  //define variable for Reos metadata
  int ncIdREOSVariable;
  error = nc_def_var( ncId, "Reos-Metadata", NC_STRING, 0, nullptr, &ncIdREOSVariable );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  const char *typeChar = "TIN";
  error = nc_put_att_text( ncId, ncIdREOSVariable, "mesh-type", std::strlen( typeChar ), typeChar );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //***********************************
  //define dimensions

  //node dimension
  int ncIdDimensionMeshNode;
  error = nc_def_dim( ncId, "TIN_Node", static_cast<size_t>( verticesCount() ), &ncIdDimensionMeshNode );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //face dimension
  int ncIdDimensionMeshFace;
  error = nc_def_dim( ncId, "TIN_Face", static_cast<size_t>( facesCount() ), &ncIdDimensionMeshFace );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }


  //max node per faces dimension
  int ncIdDimensionMaxNodesPerFaces;
  size_t maxNode = static_cast<size_t>( maxNodesPerFaces() );
  error = nc_def_dim( ncId, "Max_Node_Per_Faces", maxNode, &ncIdDimensionMaxNodesPerFaces );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //hardline dimension
  size_t hardLineVerticesCount = static_cast<size_t>( reader->hardlinesVerticesCount() );
  size_t hardLineCount = static_cast<size_t>( reader->hardlinesCount() );
  int ncIdDimensionHardLines;
  error = nc_def_dim( ncId, "HardLines_dimension", hardLineVerticesCount + hardLineCount, &ncIdDimensionHardLines );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //define variable for coordinate system
  int ncIdCRSVariable;
  error = nc_def_var( ncId, coordinateSystemVariableName.c_str(), NC_BYTE, 0, nullptr, &ncIdCRSVariable );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  std::string CRS = crs();
  const char *crsChar = CRS.c_str();
  error = nc_put_att_text( ncId, ncIdCRSVariable, "wkt", std::strlen( crsChar ), crsChar );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //define the dummy variable
  int ncIdMesh;
  error = nc_def_var( ncId, "TIN", NC_BYTE, 0, nullptr, &ncIdMesh );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  ////////////////////////////////
  // fill the attributes
  //cf role
  const char *cfRole = "mesh_topology";
  error = nc_put_att_text( ncId, ncIdMesh, "cf_role", std::strlen( cfRole ), cfRole );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //topology dimension
  int topology_dimension = 2;
  error = nc_put_att_int( ncId, ncIdMesh, "topology_dimension", NC_INT, 1, &topology_dimension );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //node Coordinate Location
  std::string ncl = meshNodeXVariableName + " " + meshNodeYVariableName;
  const char *nodeCoordinatesLocation = ncl.c_str();
  error = nc_put_att_text( ncId, ncIdMesh, "node_coordinates", std::strlen( nodeCoordinatesLocation ), nodeCoordinatesLocation );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //face node connectivity
  const char *faceNodeConnectivityLocation = meshFaceNodeConnectivity.c_str();
  error = nc_put_att_text( ncId, ncIdMesh, "face_node_connectivity", std::strlen( faceNodeConnectivityLocation ), faceNodeConnectivityLocation );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //////////////////////
  // defines and fills node coordinate variable
  int ncNodeXVariableId;
  error = nc_def_var( ncId, meshNodeXVariableName.c_str(), NC_DOUBLE, 1, &ncIdDimensionMeshNode, &ncNodeXVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  error = nc_put_att_text( ncId, ncNodeXVariableId, "grid_mapping", coordinateSystemVariableName.size(), coordinateSystemVariableName.c_str() );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int ncNodeYVariableId;
  error = nc_def_var( ncId, meshNodeYVariableName.c_str(), NC_DOUBLE, 1, &ncIdDimensionMeshNode, &ncNodeYVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  error = nc_put_att_text( ncId, ncNodeYVariableId, "grid_mapping", coordinateSystemVariableName.size(), coordinateSystemVariableName.c_str() );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int ncNodeZVariableId;
  error = nc_def_var( ncId, meshNodeZVariableName.c_str(), NC_DOUBLE, 1, &ncIdDimensionMeshNode, &ncNodeZVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }
  //fill the Z value variable attribute to be UGRID compliant
  const char *alt = "altitude";
  error = nc_put_att_text( ncId, ncNodeZVariableId, "standard_name", std::strlen( alt ), alt );
  const char *meshName = "TIN";
  error = nc_put_att_text( ncId, ncNodeZVariableId, "mesh", std::strlen( meshName ), meshName );
  const char *loc = "node";
  error = nc_put_att_text( ncId, ncNodeZVariableId, "location", std::strlen( loc ), loc );

  size_t bufferSize = 100;
  std::vector<double> nodeX( bufferSize );
  std::vector<double> nodeY( bufferSize );
  std::vector<double> nodeZ( bufferSize );

  size_t i = 0;
  while ( !reader->allVerticesReaden() )
  {

    size_t j = 0;
    while ( !reader->allVerticesReaden() && j < bufferSize )
    {
      double vert[3];
      reader->readVertex( vert );
      nodeX[j] = vert[0];
      nodeY[j] = vert[1];
      nodeZ[j] = vert[2];
      ++j;
    }
    error = nc_put_vara( ncId, ncNodeXVariableId, &i, &j, nodeX.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }

    error = nc_put_vara( ncId, ncNodeYVariableId, &i, &j, nodeY.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }

    error = nc_put_vara( ncId, ncNodeZVariableId, &i, &j, nodeZ.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }

    i += j;
  }


  //////////////////////
  // defines and fills faces variable
  int ncFacesVariableId;

  int facesDimension[2];
  facesDimension[0] = ncIdDimensionMeshFace;
  facesDimension[1] = ncIdDimensionMaxNodesPerFaces;

  error = nc_def_var( ncId, meshFaceNodeConnectivity.c_str(), NC_INT, 2, facesDimension, &ncFacesVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  //start index
  int startIndex = 0;
  error = nc_put_att_int( ncId, ncFacesVariableId, "start_index", NC_INT, 1, &startIndex );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int fillValue = -1;
  error = nc_put_att_int( ncId, ncFacesVariableId, "_FillValue", NC_INT, 1, &fillValue );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }
  std::vector<int> facesBuffer( bufferSize * maxNode );
  size_t valueCount[2];
  valueCount[1] = maxNode;
  size_t valueStart[2];
  valueStart[1] = 0;
  i = 0;
  while ( !reader->allFacesReaden() )
  {
    size_t j = 0;
    while ( !reader->allFacesReaden() && j < bufferSize )
    {
      int nodeCount;
      reader->readNodePerFace( nodeCount );
      std::vector<int> indexes( static_cast<size_t>( nodeCount ) );
      reader->readFace( indexes.data() );
      size_t k = 0;
      for ( ; k < indexes.size(); k++ )
      {
        facesBuffer[j * maxNode + k] = indexes[k];
      }

      for ( ; k < maxNode; ++k )
      {
        facesBuffer[j * maxNode + k] = fillValue;
      }
      j++;
    }
    valueCount[0] = j;
    valueStart[0] = i;
    error = nc_put_vara_int( ncId, ncFacesVariableId, valueStart, valueCount, facesBuffer.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }
    i += j;
  }


  //////////////////////
  // defines and fills neighbors variable
  int ncNeighborVariableId;

  error = nc_def_var( ncId, "face_neighbors", NC_INT, 2, facesDimension, &ncNeighborVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  error = nc_put_att_int( ncId, ncNeighborVariableId, "_FillValue", NC_INT, 1, &fillValue );
  int neighborCount;
  reader->readNodePerFace( neighborCount );
  std::vector<int> indexes( static_cast<size_t>( neighborCount ) );
  std::vector<int> neighborsBuffer( bufferSize * static_cast<size_t>( neighborCount ) );
  i = 0;
  while ( !reader->allNeighborReaden() )
  {
    size_t j = 0;
    while ( !reader->allNeighborReaden() && j < bufferSize )
    {
      reader->readNeighbor( indexes.data() );
      size_t k = 0;
      for ( ; k < indexes.size(); k++ )
      {
        if ( indexes[k] != -1 )
          neighborsBuffer[j * maxNode + k] = indexes[k];
        else
        {
          neighborsBuffer[j * maxNode + k] = fillValue;
        }
      }

      for ( ; k < maxNode; ++k )
      {
        neighborsBuffer[j * maxNode + k] = fillValue;
      }

      j++;
    }

    valueCount[0] = j;
    valueStart[0] = i;
    error = nc_put_vara_int( ncId, ncNeighborVariableId, valueStart, valueCount, neighborsBuffer.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }
    i += j;
  }

  //////////////////////
  // defines and fills hardlines variable
  int ncHardlinesVariableId;

  error = nc_def_var( ncId, "hardlines", NC_INT, 1, &ncIdDimensionHardLines, &ncHardlinesVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  i = 0;
  while ( !reader->allHardLineReaden() )
  {
    size_t verticesCount = static_cast<size_t>( reader->currentHardlineVerticesCount() );
    size_t valueCount = verticesCount + 1;
    std::vector<int> indexes( valueCount );
    indexes[0] = int( verticesCount );
    reader->readHardlineVertices( &( indexes[1] ) );
    error = nc_put_vara_int( ncId, ncHardlinesVariableId, &i, &valueCount, indexes.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }
    i += valueCount;
  }



  ////////////////////////////////////////////
  // write the zSpecifiers

  //specifier dimension
  //count the specifier to save
  size_t zSpecifierCount = static_cast<size_t>( reader->zSpecifierCount() );

  int ncIdDimensionZSpecifierCount;
  error = nc_def_dim( ncId, "ZSpecifier", zSpecifierCount, &ncIdDimensionZSpecifierCount );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  size_t maxDoubleCountPerSpecifier = 1;
  int ncIdDimensionZSpecifierMaxDouble;
  error = nc_def_dim( ncId, "ZSpecifierMaxDoubleCount", maxDoubleCountPerSpecifier, &ncIdDimensionZSpecifierMaxDouble );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  size_t maxIndexesCountPerSpecifier = 5;
  int ncIdDimensionZSpecifierMaxIndexes;
  error = nc_def_dim( ncId, "ZSpecifierMaxIndexesCount", maxIndexesCountPerSpecifier, &ncIdDimensionZSpecifierMaxIndexes );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int zSpecifierDimensionIndexes[2];
  zSpecifierDimensionIndexes[0] = ncIdDimensionZSpecifierCount;
  zSpecifierDimensionIndexes[1] = ncIdDimensionZSpecifierMaxIndexes;

  int zSpecifierDimensionDouble[2];
  zSpecifierDimensionDouble[0] = ncIdDimensionZSpecifierCount;
  zSpecifierDimensionDouble[1] = ncIdDimensionZSpecifierMaxDouble;

  int ncZSpecifierTypeVariableId;
  error = nc_def_var( ncId, "ZSpecifier type", NC_STRING, 1, &ncIdDimensionZSpecifierCount, &ncZSpecifierTypeVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int ncZSpecifierIndexesVariableId;
  error = nc_def_var( ncId, "ZSpecifier indexes", NC_INT, 2, zSpecifierDimensionIndexes, &ncZSpecifierIndexesVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  int ncZSpecifierDoubleVariableId;
  error = nc_def_var( ncId, "ZSpecifier double", NC_DOUBLE, 2, zSpecifierDimensionDouble, &ncZSpecifierDoubleVariableId );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  error = nc_put_att_int( ncId, ncZSpecifierIndexesVariableId, "_FillValue", NC_INT, 1, &fillValue );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  double doubleFillValue = INVALID_VALUE;
  error = nc_put_att_double( ncId, ncZSpecifierDoubleVariableId, "_FillValue", NC_DOUBLE, 1, &doubleFillValue );
  if ( error != NC_NOERR )
  {
    nc_close( ncId );
    return error;
  }

  i = 0;

  valueCount[1] = maxIndexesCountPerSpecifier;
  valueStart[1] = 0;

  size_t doubleValueCount[2];
  doubleValueCount[1] = maxDoubleCountPerSpecifier;
  size_t doubleValueStart[2];
  doubleValueStart[1] = 0;
  while ( ! reader->allZSpecifierReaden() )
  {
    ReosVertexZSpecifier::Data zSpecifierData;
    reader->readSpecifier( zSpecifierData );

    while ( zSpecifierData.verticesIndexes.size() < maxIndexesCountPerSpecifier )
      zSpecifierData.verticesIndexes.push_back( fillValue );

    while ( zSpecifierData.doubleData.size() < maxDoubleCountPerSpecifier )
      zSpecifierData.doubleData.push_back( doubleFillValue );

    const char *type = zSpecifierData.type.c_str();
    error = nc_put_var1_string( ncId, ncZSpecifierTypeVariableId, &i, &type );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }


    valueCount[0] = 1;
    valueStart[0] = i;
    error = nc_put_vara_int( ncId, ncZSpecifierIndexesVariableId, valueStart, valueCount, zSpecifierData.verticesIndexes.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }

    doubleValueCount[0] = 1;
    doubleValueStart[0] = i;
    error = nc_put_vara_double( ncId, ncZSpecifierDoubleVariableId, doubleValueStart, doubleValueCount, zSpecifierData.doubleData.data() );
    if ( error != NC_NOERR )
    {
      nc_close( ncId );
      return error;
    }

    i++;

  }


  return nc_close( ncId );
}

bool ReosMesh::isDirty() const {return mDirty;}

std::string ReosMesh::crs() const
{
  return mCrs;
}



MeshIO::~MeshIO() {}



