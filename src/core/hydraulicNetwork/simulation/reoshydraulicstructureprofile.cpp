#include "reoshydraulicstructureprofile.h"

#include "reoshydraulicstructure2d.h"
#include "reosgeometryutils.h"

ReosHydraulicStructureProfile::ReosHydraulicStructureProfile( const QPolygonF &geometry, ReosHydraulicStructure2D *structure )
  : QObject( structure )
  , mGeometry( geometry )
  , mStructure( structure )
{

}

QMap<double, QPolygonF> ReosHydraulicStructureProfile::parts() const
{
  if ( mParts.isEmpty() )
    initParts();
  return mParts;
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
  {
    mParts.insert( distanceFromBegining.at( i ), partsValues.at( i ) );
  }

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
