/***************************************************************************
  reosmeshgenerator.h - ReosMeshGenerator

 ---------------------
 begin                : 14.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMESHGENERATOR_H
#define REOSMESHGENERATOR_H

#include <QVector>
#include <QPolygonF>

//! Structure that contains mesh frame data
struct ReosMeshFrameData
{
  QVector<double> vertexCoordinates;
  QVector<QVector<int>> facesIndexes;
};

/**
 * Abstract class used to generate mesh frame
 */
class ReosMeshGenerator
{
  public:
    virtual ReosMeshFrameData generatedMesh() const = 0;
};

/**
 * A generator that simply triangulates a domain without internal vertex
 */
class ReosMeshGeneratorPoly2Tri : public ReosMeshGenerator
{
  public:

    virtual ReosMeshFrameData generatedMesh() const override;

    //! Sets the \a domain to triangulate
    void setDomain( const QPolygonF &domain );

  private:
    QPolygonF mDomain;
};


#endif // REOSMESHGENERATOR_H
