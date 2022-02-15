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

#include <memory>

#include <QObject>
#include <QVector>
#include <QPolygonF>

#include "reosdataobject.h"
#include "reosprocess.h"

class ReosPolylinesStructure;
class ReosParameterBoolean;
class ReosParameterDouble;
class ReosPolygonStructure;

//! Structure that contains mesh frame data
struct ReosMeshFrameData
{
  QVector<double> vertexCoordinates;
  QVector<QVector<int>> facesIndexes;
};


class ReosMeshGeneratorProcess: public ReosProcess
{
  public:

    virtual ReosMeshFrameData meshResult() const = 0;
};

/**
 * Class that controls the resolution of a generated mesh
 */
class ReosMeshResolutionController : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosMeshResolutionController( QObject *parent = nullptr, const QString &wktCrs = QString() );
    ReosMeshResolutionController( const ReosEncodedElement &element, QObject *parent = nullptr );

    ~ReosMeshResolutionController();

    ReosMeshResolutionController *clone() const;
    double elementSizeAt( double x, double y, bool exact );

    ReosPolygonStructure *resolutionPolygons() const;

    ReosParameterDouble *defaultSize() const;

    ReosEncodedElement encode() const;

  private:
    ReosMeshResolutionController( const ReosMeshResolutionController *other );
    ReosParameterDouble  *mDefaultSize = nullptr;
    std::unique_ptr<ReosPolygonStructure> mPolygonStructure;
};

/**
 * Abstract class used to generate mesh frame
 */
class ReosMeshGenerator : public QObject
{
  public:

    virtual ReosMeshGeneratorProcess *generatedMesh(
      ReosPolylinesStructure *structure,
      ReosMeshResolutionController *resolutionControler,
      bool *ok ) const = 0;

    ReosParameterBoolean *autoUpdateParameter() const;

    static ReosMeshGenerator *createMeshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );
    virtual ReosEncodedElement encode() const = 0;

  protected:
    ReosMeshGenerator( QObject *parent = nullptr );
    ReosMeshGenerator( const ReosEncodedElement &element, QObject *parent = nullptr );
    ReosEncodedElement encodeBase() const;

  private:
    ReosParameterBoolean *mAutoUpdateParameter = nullptr;
};

class ReosMeshGeneratorPoly2TriProcess: public ReosMeshGeneratorProcess
{
  public:
    ReosMeshGeneratorPoly2TriProcess( const QPolygonF &domain );

    void start();

    ReosMeshFrameData meshResult() const {return mResult;}

  private:
    QPolygonF mDomain;
    ReosMeshFrameData mResult;
};


/**
 * A generator that simply triangulates a domain without internal vertex
 */
class ReosMeshGeneratorPoly2Tri : public ReosMeshGenerator
{
  public:

    virtual ReosMeshGeneratorProcess *generatedMesh(
      ReosPolylinesStructure *structure,
      ReosMeshResolutionController *resolutionControler,
      bool *ok ) const override;

    //! Sets the \a domain to triangulate
    void setDomain( const QPolygonF &domain );

    ReosEncodedElement encode() const override;

  private:
    QPolygonF mDomain;

};


#endif // REOSMESHGENERATOR_H
