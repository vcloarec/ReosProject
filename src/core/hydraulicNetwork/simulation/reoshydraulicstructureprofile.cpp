#include "reoshydraulicstructureprofile.h"

#include "reoshydraulicstructure2d.h"
#include "reosgeometryutils.h"
#include "reosgisengine.h"

ReosHydraulicStructureProfile::ReosHydraulicStructureProfile( const QString &name, const QPolygonF &geometry, ReosHydraulicStructure2D *structure )
  : ReosDataObject( structure )
  , mGeometry( geometry )
  , mStructure( structure )
{
  setName( name );
}

ReosHydraulicStructureProfile::ReosHydraulicStructureProfile( const ReosEncodedElement &element, ReosHydraulicStructure2D *structure )
  : ReosDataObject( structure )
  , mStructure( structure )
{
  ReosDataObject::decode( element );
  element.getData( QStringLiteral( "geometry" ), mGeometry );
}

QMap<double, QPolygonF> ReosHydraulicStructureProfile::parts() const
{
  if ( mParts.isEmpty() )
    initParts();
  return mParts;
}

QMap<double, QList<ReosMeshPointValue> > ReosHydraulicStructureProfile::pointValues() const
{
  if ( mPointValues.empty() )
    buildProfile();

  return mPointValues;
}

const QPolygonF &ReosHydraulicStructureProfile::geometry() const
{
  return mGeometry;
}

QPolygonF ReosHydraulicStructureProfile::terrainProfile() const
{
  if ( mPointValues.empty() )
    buildProfile();

  std::function<double( ReosMeshPointValue )> terrainValue = [this]( const ReosMeshPointValue & points )
  {
    return points.terrainElevation( mStructure->mesh() );
  };

  return extractValue( terrainValue );
}

bool ReosHydraulicStructureProfile::hasResults( ReosHydraulicScheme *scheme ) const
{
  return mStructure->hasResults( scheme );
}

QPolygonF ReosHydraulicStructureProfile::resultsProfile( ReosHydraulicScheme *scheme, int datasetIndex, ReosHydraulicSimulationResults::DatasetType resultType ) const
{
  if ( datasetIndex == -1 )
    return QPolygonF();
  if ( mPointValues.empty() )
    buildProfile();

  ReosHydraulicSimulationResults *results = mStructure->results( scheme );

  if ( results )
  {
    int groupIndex = results->groupIndex( resultType );
    std::function<double( ReosMeshPointValue )> resultValue = [results, groupIndex, datasetIndex]( const ReosMeshPointValue & points )
    {
      return points.value( results, groupIndex, datasetIndex );
    };

    return extractValue( resultValue );
  }

  return QPolygonF();
}

QPolygonF ReosHydraulicStructureProfile::resultsProfile( ReosHydraulicScheme *scheme, const QDateTime &time, ReosHydraulicSimulationResults::DatasetType resultType ) const
{
  ReosHydraulicSimulationResults *results = mStructure->results( scheme );
  if ( results )
  {
    int groupIndex = results->groupIndex( resultType );
    int datasetindex = results->datasetIndex( groupIndex, time );
    return resultsProfile( scheme, datasetindex, resultType );
  }

  return QPolygon();

}

static QPolygonF mergeWaterlevelWithTerrain( const QPolygonF &waterLevel, const QPolygonF &terrain, QPolygonF &correctedWaterSurface )
{
  if ( waterLevel.count() < 2 || terrain.count() < 2 )
    return QPolygonF();

  QPolygonF wsPart = waterLevel;
  QPolygonF zPart = terrain;

  bool removeFirst = false;
  bool removeLast = false;

  if ( wsPart.last().y() < zPart.last().y() )
  {
    QPointF intersection;
    ReosGeometryUtils::segmentIntersect( wsPart.last(), wsPart.at( wsPart.count() - 2 ), zPart.last(), zPart.at( zPart.count() - 2 ), intersection );
    wsPart.removeLast();
    wsPart.append( intersection );
  }

  if ( wsPart.first().y() < zPart.first().y() )
  {
    QPointF intersection;
    ReosGeometryUtils::segmentIntersect( wsPart.first(), wsPart.at( 1 ), zPart.first(), zPart.at( 1 ), intersection );
    wsPart.removeFirst();
    wsPart.insert( 0, intersection );
  }

  if ( removeFirst )
    zPart.removeFirst();

  if ( removeLast )
    zPart.removeLast();

  for ( int i = 0; i < wsPart.count(); ++i )
    zPart.append( wsPart.at( wsPart.size() - 1 - i ) );

  correctedWaterSurface = wsPart;
  double lastX = correctedWaterSurface.last().x();
  correctedWaterSurface.append( QPointF( lastX + lastX * 10 * std::numeric_limits<double>::epsilon(), std::numeric_limits<double>::quiet_NaN() ) );

  return zPart;
}

QList<QPolygonF> ReosHydraulicStructureProfile::resultsFilledByWater( ReosHydraulicScheme *scheme, const QDateTime &time, QPolygonF &totalWaterSurface ) const
{
  if ( mPointValues.empty() )
    buildProfile();

  QList<QPolygonF> ret;
  ReosHydraulicSimulationResults *results = mStructure->results( scheme );
  if ( !results )
    return ret;

  int groupIndex = results->groupIndex( ReosHydraulicSimulationResults::DatasetType::WaterLevel );
  int datasetindex = results->datasetIndex( groupIndex, time );
  if ( datasetindex == -1 )
    return QList<QPolygonF>();


  totalWaterSurface.clear();

  std::function<double( ReosMeshPointValue )> waterLevelValue = [results, groupIndex, datasetindex]( const ReosMeshPointValue & points )
  {
    return points.value( results, groupIndex, datasetindex );
  };

  std::function<double( ReosMeshPointValue )> terrainValue = [this]( const ReosMeshPointValue & points )
  {
    return points.terrainElevation( mStructure->mesh() );
  };

  const QString &crs = mStructure->mesh()->crs();
  for ( auto it = mPointValues.constBegin(); it != mPointValues.constEnd(); ++it )
  {
    QPolygonF waterSurface;
    QPolygonF terrain;
    double posInLine = 0;
    const QList<ReosMeshPointValue> &values = it.value();

    if ( values.count() < 2 )
      continue;

    posInLine = ReosGisEngine::locateOnPolyline( values.first().position(), mGeometry, crs );

    bool inWater = false;

    double waterLevel0 = waterLevelValue( values.at( 0 ) );
    double zTerrain0 = terrainValue( values.at( 0 ) );
    if ( std::isnan( waterLevel0 ) )
      waterLevel0 = zTerrain0 - 1;

    if ( waterLevel0 > zTerrain0 )
    {
      inWater = true;
      waterSurface.append( QPointF( posInLine, waterLevel0 ) );
      terrain.append( QPointF( posInLine, zTerrain0 ) );
    }
    else if ( waterLevel0 < zTerrain0 )
    {
      inWater = false;
    }
    else //water level == terrain
    {
      double waterLevel1 = waterLevelValue( values.at( 1 ) );
      double zTerrain1 = terrainValue( values.at( 1 ) );

      if ( waterLevel1 > zTerrain1 )
      {
        inWater = true;
        waterSurface.append( QPointF( posInLine, waterLevel0 ) );
        terrain.append( QPointF( posInLine, zTerrain0 ) );
      }
      else
      {
        inWater = false;
      }
    }

    double posInLine0 = posInLine;
    for ( int i = 1; i < values.count(); ++i )
    {
      const ReosMeshPointValue &point = values.at( i );
      posInLine += ReosGisEngine::distance( point.position(), values.at( i - 1 ).position(), crs );

      double w1 = waterLevelValue( point );
      double z1 = terrainValue( point );
      if ( std::isnan( w1 ) )
        w1 = z1 - 1.0;

      if ( inWater )
      {
        waterSurface.append( QPointF( posInLine, w1 ) );
        terrain.append( QPointF( posInLine, z1 ) );

        if ( z1 > w1 )
        {
          inWater = false;
          QPolygonF corWs;
          ret.append( mergeWaterlevelWithTerrain( waterSurface, terrain, corWs ) );
          totalWaterSurface.append( corWs );
          waterSurface.clear();
          terrain.clear();
        }
        else if ( z1 == w1 )
        {
          QPolygonF corWs;
          ret.append( mergeWaterlevelWithTerrain( waterSurface, terrain, corWs ) );
          totalWaterSurface.append( corWs );
          waterSurface.clear();
          terrain.clear();

          if ( i < values.count() - 1 )
          {
            double w2 = waterLevelValue( values.at( i + 1 ) );
            double z2 = terrainValue( values.at( i + 1 ) );
            if ( std::isnan( w2 ) )
              w2 = z2 - 1;

            if ( w2 > z2 )
            {
              inWater = true;
              waterSurface.append( QPointF( posInLine, w1 ) );
              terrain.append( QPointF( posInLine, z1 ) );
            }
            else
            {
              inWater = false;
            }
          }
        }
      }
      else // not in water
      {
        if ( w1 > z1 )
        {
          double w0 = waterLevelValue( values.at( i - 1 ) );
          double z0 = terrainValue( values.at( i - 1 ) );
          if ( !std::isnan( w0 ) )
          {
            waterSurface.append( QPointF( posInLine0, w0 ) );
            terrain.append( QPointF( posInLine0, z0 ) );
          }
          waterSurface.append( QPointF( posInLine, w1 ) );
          terrain.append( QPointF( posInLine, z1 ) );
          inWater = true;
        }
        else if ( w1 == z1 )
        {
          if ( i < values.count() - 1 )
          {
            double w2 = waterLevelValue( values.at( i + 1 ) );
            double z2 = terrainValue( values.at( i + 1 ) );
            if ( std::isnan( w2 ) )
              w2 = z2 - 1;

            if ( w2 > z2 )
            {
              inWater = true;
              waterSurface.append( QPointF( posInLine, w1 ) );
              terrain.append( QPointF( posInLine, z1 ) );
            }
          }
        }

      }

      posInLine0 = posInLine;
    }

    QPolygonF corWs;
    ret.append( mergeWaterlevelWithTerrain( waterSurface, terrain, corWs ) );
    totalWaterSurface.append( corWs );
  }

  return ret;
}

void ReosHydraulicStructureProfile::changeGeometry( const QPolygonF &geom, const QString &linesCrs )
{
  mParts.clear();
  mPointValues.clear();
  mGeometry = ReosGisEngine::transformToCoordinates( linesCrs, geom, mStructure->mesh()->crs() );
}


QRectF ReosHydraulicStructureProfile::elevationExtent( ReosHydraulicScheme *scheme ) const
{
  QRectF ret = terrainExtent();

  if ( mStructure->hasResults( scheme ) )
  {
    ReosHydraulicSimulationResults *results = mStructure->results( scheme );
    if ( results )
    {
      int waterLevelIndex = results->groupIndex( ReosHydraulicSimulationResults::DatasetType::WaterLevel );
      int timeStepCount = results->datasetCount( waterLevelIndex );

      bool ok = false;
      for ( int i = 0; i < timeStepCount; ++i )
      {
        QRectF wsExt = ReosGeometryUtils::boundingBox( resultsProfile( scheme, i, ReosHydraulicSimulationResults::DatasetType::WaterLevel ), ok );
        if ( ok )
          ret = ret.united( wsExt );
      }
    }

  }

  return ret;
}

QPair<double, double> ReosHydraulicStructureProfile::valueVerticalExtent( ReosHydraulicScheme *scheme, ReosHydraulicSimulationResults::DatasetType resultType )
{
  ReosHydraulicSimulationResults *results = mStructure->results( scheme );

  if ( !results )
    return QPair<double, double>( 0, 0 );

  int waterLevelIndex = results->groupIndex( resultType );
  int timeStepCount = results->datasetCount( waterLevelIndex );

  QRectF ext;

  bool ok = false;
  for ( int i = 0; i < timeStepCount; ++i )
  {
    QRectF wsExt = ReosGeometryUtils::boundingBox( resultsProfile( scheme, i, resultType ), ok );
    if ( ok )
      ext = ext.united( wsExt );
  }

  return QPair<double, double>( ext.top(), ext.bottom() );
}

ReosEncodedElement ReosHydraulicStructureProfile::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-structure-profile" ) );
  ReosDataObject::encode( element );

  element.addData( QStringLiteral( "geometry" ), mGeometry );

  return element;
}

void ReosHydraulicStructureProfile::initParts() const
{
  const QPolygonF &domain = mStructure->domain();

  const ReosPolylinesStructure::Data &data = mStructure->geometryStructure()->structuredLinesData();

  const QVector<QPointF> &vertices = data.vertices;
  const QVector<QVector<int>> &internalLines = data.internalLines;
  const QVector<QVector<int>> &holes = data.holes;

  mParts.clear();

  QVector<double> distanceFromBegining;
  QVector<QPolygonF> partsValues = ReosGeometryUtils::cutPolylineOutsidePolygon( mGeometry, domain, &distanceFromBegining );
  Q_ASSERT( distanceFromBegining.count() == partsValues.count() );

  double dist0 = 0;
  if ( !partsValues.isEmpty() )
  {
    const QPolygonF &firstPoly = partsValues.first();
    if ( !firstPoly.empty() )
    {
      QPointF vect0 = mGeometry.first() - firstPoly.first();
      dist0 = sqrt( pow( vect0.x(), 2.0 ) + pow( vect0.y(), 2.0 ) );
    }
  }

  for ( int i = 0; i < partsValues.count(); ++i )
    mParts.insert( dist0 + distanceFromBegining.at( i ), partsValues.at( i ) );

  if ( !holes.isEmpty() )
  {
    QMap<double, QPolygonF> newParts;

    for ( int pi = 0; pi < mParts.count(); ++pi )
    {
      const QPolygonF &oldPart = partsValues.at( pi );
      double dist = distanceFromBegining.at( pi );
      for ( const QVector<int> &holeIndex : holes )
      {
        QPolygonF hole( holeIndex.size() );
        for ( int i = 0; i < hole.size(); ++i )
        {
          int index = holeIndex.at( i );
          hole[i] = vertices.at( internalLines.at( index ).first() ) ;
        }

        QVector<double> holeDistanceFromBegining;
        QVector<QPolygonF> newValues = ReosGeometryUtils::cutPolylineInsidePolygon( oldPart, hole, &holeDistanceFromBegining );
        Q_ASSERT( holeDistanceFromBegining.count() == newValues.count() );
        for ( int i = 0; i < newValues.count(); ++i )
        {
          newParts.insert( dist + holeDistanceFromBegining.at( i ), newValues.at( i ) );
        }
      }
    }

    mParts = newParts;
  }
}

void ReosHydraulicStructureProfile::buildProfile() const
{
  const QMap<double, QPolygonF> allParts = parts();
  mPointValues.clear();
  for ( auto it = allParts.constBegin(); it != allParts.constEnd(); ++it )
  {
    mPointValues.insert( it.key(), mStructure->mesh()->drapePolyline( it.value(), 0.0000001 ) );
  }
  bool ok = false;
  mTerrainExtent = ReosGeometryUtils::boundingBox( terrainProfile(), ok );
}

QPolygonF ReosHydraulicStructureProfile::extractValue( std::function<double ( ReosMeshPointValue )> &func ) const
{
  QPolygonF ret;
  const QString &crs = mStructure->mesh()->crs();
  for ( auto it = mPointValues.constBegin(); it != mPointValues.constEnd(); ++it )
  {
    double posInLine = 0;
    const QList<ReosMeshPointValue> &values = it.value();
    if ( !values.isEmpty() )
      posInLine = ReosGisEngine::locateOnPolyline( values.first().position(), mGeometry, crs );

    for ( int i = 0; i < values.count(); ++i )
    {
      const ReosMeshPointValue &points = values.at( i );
      if ( i > 0 )
        posInLine += ReosGisEngine::distance( points.position(), values.at( i - 1 ).position(), crs );
      ret.append( QPointF( posInLine, func( points ) ) );
    }

    if ( !values.isEmpty() )
      ret.append( QPointF( ret.last().x() * ( 1 + 10 * std::numeric_limits<double>::epsilon() ), std::numeric_limits<double>::quiet_NaN() ) );
  }

  return ret;
}

QRectF ReosHydraulicStructureProfile::terrainExtent() const
{
  if ( mPointValues.empty() )
    buildProfile();

  return mTerrainExtent;

}

ReosHydraulicStructureProfilesCollection::ReosHydraulicStructureProfilesCollection( QObject *parent ): QAbstractListModel( parent )
{

}

QModelIndex ReosHydraulicStructureProfilesCollection::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosHydraulicStructureProfilesCollection::parent( const QModelIndex & ) const
{return QModelIndex(); }

int ReosHydraulicStructureProfilesCollection::rowCount( const QModelIndex & ) const
{
  return mProfiles.count();
}

int ReosHydraulicStructureProfilesCollection::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosHydraulicStructureProfilesCollection::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() >= mProfiles.count() )
    return QVariant();

  if ( role == Qt::DisplayRole )
    return mProfiles.at( index.row() )->name();

  return QVariant();
}

void ReosHydraulicStructureProfilesCollection::addProfile( ReosHydraulicStructureProfile *profile )
{
  beginInsertRows( QModelIndex(), mProfiles.count(), mProfiles.count() );
  mProfiles.append( profile );
  endInsertRows();
}

void ReosHydraulicStructureProfilesCollection::removeProfile( int index )
{
  beginRemoveRows( QModelIndex(), index, index );
  mProfiles.takeAt( index )->deleteLater();
  endRemoveRows();
}

void ReosHydraulicStructureProfilesCollection::renameProfile( int profileIndex, const QString &name )
{
  mProfiles.at( profileIndex )->setName( name );
  emit dataChanged( index( profileIndex, 0, QModelIndex() ), index( profileIndex, 0, QModelIndex() ) );
}

ReosHydraulicStructureProfile *ReosHydraulicStructureProfilesCollection::profile( int profileIndex ) const
{
  if ( profileIndex < 0 || profileIndex >= mProfiles.count() )
    return nullptr;

  return mProfiles.at( profileIndex );
}

int ReosHydraulicStructureProfilesCollection::profileIndex( ReosHydraulicStructureProfile *profile ) const
{
  return mProfiles.indexOf( profile );
}

ReosEncodedElement ReosHydraulicStructureProfilesCollection::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-structure-profiles-collection" ) );

  QList<ReosEncodedElement> encodedProfiles;

  for ( ReosHydraulicStructureProfile *profile : mProfiles )
    encodedProfiles.append( profile->encode() );

  element.addListEncodedData( QStringLiteral( "profiles" ), encodedProfiles );

  return element;
}

void ReosHydraulicStructureProfilesCollection::decode( const ReosEncodedElement &element, ReosHydraulicStructure2D *structure )
{
  if ( element.description() != QStringLiteral( "hydraulic-structure-profiles-collection" ) )
    return;

  beginResetModel();
  QList<ReosEncodedElement> encodedProfiles = element.getListEncodedData( QStringLiteral( "profiles" ) );

  for ( const ReosEncodedElement &elem : encodedProfiles )
    mProfiles.append( new ReosHydraulicStructureProfile( elem, structure ) );

  endResetModel();
}
