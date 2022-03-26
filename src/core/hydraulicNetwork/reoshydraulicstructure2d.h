/***************************************************************************
  reoshydraulicstructure2d.h - ReosHydraulicStructure2D

 ---------------------
 begin                : 9.1.2022
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
#ifndef REOSHYDRAULICSTRUCTURE2D_H
#define REOSHYDRAULICSTRUCTURE2D_H

#include "reoshydraulicnetwork.h"
#include "reospolylinesstructure.h"
#include "reosgmshgenerator.h"
#include "reosmesh.h"
#include "reos3dmapsettings.h"

class ReosTopographyCollection;
class ReosRoughnessStructure;
class ReosHydraulicStructureBoundaryCondition;
class ReosHydraulicSimulation;
class QDir;

class ReosHydraulicStructure2D : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    struct BoundaryVertices
    {
      QVector<int> verticesIndex;
      QPointer<ReosHydraulicStructureBoundaryCondition> boundaryCondition;
    };


    ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context );

    static ReosHydraulicStructure2D *create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext  &context );
    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "structure2D" );}

    //! Returns the domain polygon
    QPolygonF domain( const QString &crs = QString() ) const;

    ReosPolylinesStructure *geometryStructure() const;

    ReosMeshResolutionController *meshResolutionController() const;

    ReosMesh *mesh() const;

    ReosMeshGenerator *meshGenerator() const;

    ReosMeshGeneratorProcess *getGenerateMeshProcess();

    QVector<BoundaryVertices> boundaryVertices() const;

    //! Sets active the terrain in the mesh
    void activateMeshTerrain();

    //! Return the id of the terrain dataset
    QString terrainMeshDatasetId() const;

    //! Deactivate any activated scalar dataset
    void deactivateMeshScalar();

    void runSimulation();

    ReosTopographyCollection *topographyCollecion() const;

    Reos3DMapSettings map3dSettings() const;
    void setMap3dSettings( const Reos3DMapSettings &value );

    Reos3DTerrainSettings terrain3DSettings() const;
    void setTerrain3DSettings( const Reos3DTerrainSettings &settings );

    ReosRoughnessStructure *roughnessStructure() const;

    QDir structureDirectory();

    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydraulicStructureBoundaryCondition *boundaryCondition, bool upstreamWillChange ) {}
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context ) {}

  public slots:
    void updateCalculationContext( const ReosCalculationContext &context );

  signals:
    void meshGenerated();

  protected:
    void encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const;

  private slots:
    void onBoundaryConditionAdded( const QString &bid );
    void onBoundaryConditionRemoved( const QString &bid );
    void onGeometryStructureChange();

  private:
    ReosHydraulicStructure2D( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    ReosMeshGenerator *mMeshGenerator = nullptr;
    std::unique_ptr<ReosPolylinesStructure> mPolylinesStructures;
    ReosMeshResolutionController *mMeshResolutionController = nullptr;
    ReosTopographyCollection  *mTopographyCollecion = nullptr;
    std::unique_ptr<ReosMesh> mMesh;
    std::unique_ptr<ReosRoughnessStructure > mRoughnessStructure;
    QVector<QVector<int>> mBoundaryVertices;
    QVector<QVector<QVector<int>>> mHolesVertices;

    ReosHydraulicSimulation *mSimulation = nullptr;

    QString mTerrainDatasetId;
    Reos3DMapSettings m3dMapSettings;
    Reos3DTerrainSettings m3dTerrainSettings;

    ReosHydraulicNetworkContext mHydraulicNetworkContext;

    void init();
    void generateMeshInPlace();
    QString directory() const;
    ReosHydraulicStructureBoundaryCondition *boundaryConditionNetWorkElement( const QString boundaryId ) const;
    void onMeshGenerated( const ReosMeshFrameData &meshData );
};

class ReosHydraulicStructure2dFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicStructure2dFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};


class ReosRoughnessStructure : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosRoughnessStructure( const QString &mCrs );
    ReosRoughnessStructure( const ReosEncodedElement &encodedElement );

    ReosEncodedElement encode() const;
    ReosParameterDouble *defaultRoughness() const;
    ReosPolygonStructure *structure() const;

  private:
    std::unique_ptr<ReosPolygonStructure> mStructure;
    ReosParameterDouble *mDefaultRoughness = nullptr;
};


#endif // REOSHYDRAULICSTRUCTURE2D_H
