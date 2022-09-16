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

QPolygonF ReosHydraulicStructureProfile::resultsProfile( ReosHydraulicScheme *scheme, const QDateTime &time, ReosHydraulicSimulationResults::DatasetType resultType ) const
{
  if ( mPointValues.empty() )
    buildProfile();

  ReosHydraulicSimulationResults *results = mStructure->results( scheme );
  int groupIndex = results->groupIndex( resultType );
  int datasetindex = results->datasetIndex( groupIndex, time );
  if ( datasetindex == -1 )
    return QPolygonF();

  std::function<double( ReosMeshPointValue )> resultValue = [results, groupIndex, datasetindex]( const ReosMeshPointValue & points )
  {
    return points.value( results, groupIndex, datasetindex );
  };

  return extractValue( resultValue );
}

void ReosHydraulicStructureProfile::changeGeometry( const QPolygonF &geom, const QString &linesCrs )
{
  mParts.clear();
  mPointValues.clear();
  mGeometry = ReosGisEngine::transformToCoordinates( linesCrs, geom, mStructure->mesh()->crs() );
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
    mPointValues.insert( it.key(), mStructure->mesh()->drapePolyline( it.value(), 0.0000001 ) );
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

ReosHydraulicStructureProfile *ReosHydraulicStructureProfilesCollection::profile( int profileIndex )
{
  if ( profileIndex < 0 || profileIndex >= mProfiles.count() )
    return nullptr;

  return mProfiles.at( profileIndex );
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
