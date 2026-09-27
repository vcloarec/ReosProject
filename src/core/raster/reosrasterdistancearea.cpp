#include "reosrasterdistancearea.h"
#include <qgscoordinatereferencesystem.h>
#include <qgsprojectionfactors.h>
#include <qgspoint.h>

ReosRasterDistanceArea::ReosRasterDistanceArea()
{}

ReosRasterDistanceArea::ReosRasterDistanceArea( const ReosRasterExtent extent, int reductionFactor )
  : mExtent( extent )
  , mReductionFactor( reductionFactor )
{
  initGrids();
}

void ReosRasterDistanceArea::adjustDistance( float &xDistance, float &yDistance, int row, int col ) const
{
  int row0 = static_cast<int>( std::floor( row / mReductionFactor ) );
  int col0 = static_cast<int>( std::floor( col / mReductionFactor ) );

  if ( row0 < 0 || row0 >= mXDistanceScale.rowCount() || col0 < 0 || col0 >= mXDistanceScale.columnCount() )
    return;

  xDistance = xDistance * mXDistanceScale.value( row0, col0 );
  yDistance = yDistance * mYDistanceScale.value( row0, col0 );
}

float ReosRasterDistanceArea::areaFactor( int row, int col ) const
{
  int row0 = static_cast<int>( std::floor( row / mReductionFactor ) );
  int col0 = static_cast<int>( std::floor( col / mReductionFactor ) );

  if ( row0 < 0 || row0 >= mXDistanceScale.rowCount() || col0 < 0 || col0 >= mXDistanceScale.columnCount() )
    return 1.0;

  return mXDistanceScale.value( row0, col0 ) * mYDistanceScale.value( row0, col0 );
}

bool ReosRasterDistanceArea::isValid() const
{
  return mIsvalid;
}

void ReosRasterDistanceArea::initGrids()
{
  QgsCoordinateReferenceSystem qgsCrs = QgsCoordinateReferenceSystem::fromWkt( mExtent.crs() );

  int rowCount = static_cast<int>( std::floor( mExtent.yCellCount() / mReductionFactor ) ) + 1;
  int colCount = static_cast<int>( std::floor( mExtent.xCellCount() / mReductionFactor ) ) + 1;

  mXDistanceScale.reserveMemory( rowCount, colCount );
  mYDistanceScale.reserveMemory( rowCount, colCount );

  double xOrigin = mExtent.xMapOrigin();
  double yOrigin = mExtent.yMapOrigin();

  double reductedXSize = mExtent.xCellSize() * mReductionFactor;
  double reductedYSize = mExtent.yCellSize() * mReductionFactor;

  for ( int r = 0; r < rowCount; r++ )
    for ( int c = 0; c < colCount; c++ )
    {
      QgsPoint point( QgsPointXY( xOrigin + 0.5 * c * reductedXSize, yOrigin - 0.5 * r * reductedYSize ) );
      QgsProjectionFactors factors = qgsCrs.factors( point );

      if ( factors.isValid() )
      {
        mXDistanceScale.setValue( r, c, 1 / factors.meridionalScale() );
        mYDistanceScale.setValue( r, c, 1 / factors.parallelScale() );
      }
      else
      {
        mXDistanceScale.setValue( r, c, 1 );
        mYDistanceScale.setValue( r, c, 1 );
      }
    }

  mIsvalid = true;
}
