#include "reoshydraulicstructureprofile.h"

#include "reoshydraulicstructure2d.h"
#include "reosgeometryutils.h"

ReosHydraulicStructureProfile::ReosHydraulicStructureProfile( const QString &name, const QPolygonF &geometry, ReosHydraulicStructure2D *structure )
  : ReosDataObject( structure )
  , mGeometry( geometry )
  , mStructure( structure )
{
  setName( name );
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
  for ( int i = 0; i < partsValues.count(); ++i )
    mParts.insert( distanceFromBegining.at( i ), partsValues.at( i ) );


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

ReosHydraulicStructureProfile *ReosHydraulicStructureProfilesCollection::profile( int profileIndex )
{
  if ( profileIndex < 0 || profileIndex >= mProfiles.count() )
    return nullptr;

  return mProfiles.at( profileIndex );
}
