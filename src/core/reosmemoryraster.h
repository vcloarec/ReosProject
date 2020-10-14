/***************************************************************************
                      reosmemoryraster.h
                     --------------------------------------
Date                 : 23-05-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMEMORYRASTER_H
#define REOSMEMORYRASTER_H

#include <array>

#include <QPoint>
#include <QPointF>
#include <QRectF>
#include <QPolygonF>

#include <gdal/gdal_priv.h>
#include <gdal/ogr_spatialref.h>

#include "reosmapextent.h"

class ReosRasterCellPos;

/**
 * Class that represent the extent of a raster in a map, handle also pixel map position
 */
class ReosRasterExtent : public ReosMapExtent
{
  public:

    enum Position
    {
      Center,
      Interior,
      Exterior
    };

    //! Default constructor
    ReosRasterExtent() = default;

    /**
     * Constructor
     *
     * \param extent in real world coordinate coordinates
     * \param XCellCount
     * \param XCellSize size of the cell in X direction, can be negative
     * \param YCellSize size of the cell in X direction, can be negative
     *
     * If cell size is negative, the row or column index will increase when real world coordinate decreases.
     * So ,if YCellSize < 0 and XCellSize>0, then the origin of the grid raster (0,0) will be at (Xmin,Ymax) of extent.
     *
     */
    ReosRasterExtent( double xOrigine, double yOrigine, int XCellCount, int YCellCount, double XCellSize, double YCellSize );
    ReosRasterExtent( const ReosMapExtent &extent, int XCellCount, int YcellCount, bool xAscendant = true, bool yAscendant = false );

    //! Returns whether the extent is valid
    bool isValid() const;
    //! Returns x map coordinate of the orgin if the raster, x min if XCellSize > 0
    double xMapOrigin() const;
    //! Returns y map coordinate of the orgin if the raster, x min if XCellSize > 0
    double yMapOrigin() const;
    //! Returns cell size in X direction
    double xCellSize() const;
    //! Returns cell size in Y direction
    double yCellSize() const;
    //! Returns cell count in X direction
    int xCellCount() const;
    //! Returns cell count in Y direction
    int yCellCount() const;

    //! Returns position of the cell from a \a point in real world coordinates
    QPoint mapToCell( const QPointF &point ) const;
    //! Returns the position in real world coordinate of the corner of cell (min x and min y if cell size >0)
    QPointF interCellToMap( const QPoint &cellPos ) const;
    //! Returns the x coordinate before the column i
    double cellXBeforeToMap( int i ) const;
    //! Returns the x coordinate after the column i
    double cellXAfterToMap( int i ) const;
    //! Returns the y coordinate before the row i
    double cellYBeforeToMap( int i ) const;
    //! Returns the y coordinate after the row i
    double cellYAfterToMap( int i )const ;
    //! Returns the position in real world coordinate of the corner of cell (min x and min y if cell size >0)
    QPointF cellMinMinCornerToMap( const QPoint &cellPos ) const;
    //! Returns the position in real world coordinate of the corner of cell (max x and max y if cell size >0)
    QPointF cellMaxMaxCornerToMap( const QPoint &cellPos ) const;
    //! Returns the position in real world coordinate of the corner of cell (min x and min y if cell size >0)
    QPointF cellMinMaxCornerToMap( const QPoint &cellPos ) const;
    //! Returns the position in real world coordinate of the corner of cell (max x and max y if cell size >0)
    QPointF cellMaxMinCornerToMap( const QPoint &cellPos ) const;
    //! Returns the position in real world coordinate of the center of the cell at position \a cellPos
    QPointF cellCenterToMap( const QPoint &cellPos ) const;
    //! Returns the position in real world coordinate of the center of the cell at position \a cellPos
    QPointF cellCenterToMap( const ReosRasterCellPos &cellPos ) const;
    //! Returns a rectangle from real world cordintates to raster cell postions
    QRect mapExtentToCellRect( const ReosMapExtent &mapExtent ) const;
    //! Returns a rectangle from raster cell position to real world coordinates
    ReosMapExtent cellRectToMapExtent( const QRect &cellRect, const Position &position = Center ) const;
    //! Returns the surface of a cell
    double cellSurface() const;
    //! Returns position of the cell from a \a point in real world coordinates
    ReosRasterCellPos mapToCellPos( const QPointF &point ) const;

    //! Returns the intersection of the extents, the position and the size of the pixels are the ones of the first member
    ReosRasterExtent operator*( const ReosRasterExtent &other ) const;


  private:
    bool mIsValid = false;
    double mXOrigin = std::numeric_limits<double>::quiet_NaN();
    double mYOrigin = std::numeric_limits<double>::quiet_NaN();
    double mXCellSize = std::numeric_limits<double>::quiet_NaN();
    double mYCellSize = std::numeric_limits<double>::quiet_NaN();
    int mXCellCount = 0;
    int mYCellCount = 0;


};

/**
 * Convenient class used to navigate in a raster
 */
class ReosRasterCellPos
{
  public:
    //! Default constructor
    ReosRasterCellPos() = default;
    //! Constructor
    ReosRasterCellPos( int r, int c );

    //! Returns the row index
    int row() const;
    //! Sets the row index
    void setRow( int value );
    //! Returns the column index
    int column() const;
    //! Sets the column index
    void setColumn( int value );

    ReosRasterCellPos operator+( const ReosRasterCellPos &other ) const;
    ReosRasterCellPos operator-( const ReosRasterCellPos &other ) const;

    bool operator==( const ReosRasterCellPos &other ) const;
    bool operator!=( const ReosRasterCellPos &other ) const;

    /**
     * Return new position from this position and a direction :
     *
    // -------------
    // | 0 | 3 | 6 |
    // -------------
    // | 1 | 4 | 7 |
    // -------------
    // | 2 | 5 | 8 |
    // -------------
     *
     */
    ReosRasterCellPos neighbourWithDirection( unsigned char direction );

    void goInDirection( unsigned char direction );

    virtual bool isValid() const
    {
      return mRow != -1 && mColumn != -1;
    }

  private:
    int mRow = -1;
    int mColumn = -1;
};


/**
 * Class that stores a raster of type T in memory
 */
template <typename T>
class ReosRasterMemory
{
  public:
    //! Default constructor, empty raster
    ReosRasterMemory() = default;
    //! Constructor with  \a nb_row, and \a nb_col, the row count and the column count, do not reserve memory
    ReosRasterMemory( int nb_row, int nb_col );
    //! Reserves memory with dimension used in constructor, returns true if successful
    bool reserveMemory();
    //! Reserves memory with dimension \a nb_row and \a nb_col, returns true if successful
    bool reserveMemory( int nb_row, int nb_col );
    //! Clears and frees memory
    bool freeMemory();
    //! Returns the value at position \a i,j
    T value( int row, int col ) const;
    //! Sets the value at position \a i,j
    void setValue( int row, int col, T v );
    //! Sets the value at position \a cellPos
    void setValue( const ReosRasterCellPos &cellPos, T v );
    //! Returns a void pointer to the data
    void *data();
    //! Sets the value that is considered as no data
    void setNodata( T nd );
    //! Returns the value that is considered as no data
    T noData() const;
    //! Returns the row count
    int rowCount() {return mRowCount;}
    //! Returns the columns count
    int columnCount() {return mColumnCount;}
    //! Fill the raster with \a val
    void fill( T val );

    //! Load the data from a Tiff file using GDAL
    bool loadDataFromTiffFile( const char *fileName, GDALDataType type );
    //! Creates a Tiff file with data using GDAL
    bool createTiffFile( const char *fileName, GDALDataType type, double *geoTrans, OGRSpatialReference *crs = nullptr );
    //! Creates a Tiff file with data using GDAL
    bool createTiffFile( const char *fileName, GDALDataType type, const ReosRasterExtent &emprise, OGRSpatialReference *crs = nullptr );

    //! Copies the data from \a other
    bool copy( ReosRasterMemory<T> *other );

    //! Returns whether the raster is valid
    bool isValid() const;

    //! Returns a pointer to a new raster in memory from \a this with reduced column and row count, the caller takes ownership
    ReosRasterMemory<T> *reduceRaster( int rowMin, int rowMax, int columnMin, int columnMax );

    bool operator==( const ReosRasterMemory<T> &rhs ) const;

  private:
    int mRowCount = 0;
    int mColumnCount = 0;;
    QVector<T> mValues;
    T mNoData;
};

template<typename T>
void *ReosRasterMemory<T>::data() {return mValues.data();}

template<typename T>
void ReosRasterMemory<T>::setNodata( T nd ) {mNoData = nd;}

template<typename T>
T ReosRasterMemory<T>::noData() const {return mNoData;}


template<typename T>
ReosRasterMemory<T>::ReosRasterMemory( int nb_row, int nb_col ):
  mRowCount( nb_row ), mColumnCount( nb_col )
{

}

template<typename T>
bool ReosRasterMemory<T>::reserveMemory()
{
  if ( mRowCount * mColumnCount == 0 )
    return false;

  mValues.clear();
  try
  {
    mValues.resize( mRowCount * mColumnCount );
  }
  catch ( std::bad_alloc )
  {
    return false;
  }
  return true;
}

template<typename T>
bool ReosRasterMemory<T>::reserveMemory( int nb_row, int nb_col )
{
  mRowCount  = nb_row;
  mColumnCount = nb_col;
  return reserveMemory();
}

template<typename T>
bool ReosRasterMemory<T>::freeMemory()
{
  mValues.clear();
  return true;
}

template<typename T>
T ReosRasterMemory<T>::value( int row, int col ) const
{
  if ( ( row < 0 ) || ( row >= mRowCount ) || ( col < 0 ) || ( col >= mColumnCount ) )
    return noData();

  return mValues[row * mColumnCount + col];
}

template<typename T>
void ReosRasterMemory<T>::setValue( int row, int col, T v )
{
  if ( ( row < mRowCount ) && ( col < mColumnCount ) && ( row >= 0 ) && ( col >= 0 ) )
    mValues[row * mColumnCount  + col] = v;
}

template<typename T>
void ReosRasterMemory<T>::setValue( const ReosRasterCellPos &cellPos, T v )
{
  setValue( cellPos.row(), cellPos.column(), v );
}

template<typename T>
void ReosRasterMemory<T>::fill( T val )
{
  for ( int i = 0; i < mRowCount; ++i )
    for ( int j = 0; j < mColumnCount; ++j )
      setValue( i, j, val );
}


template<typename T>
bool ReosRasterMemory<T>::loadDataFromTiffFile( const char *fileName, GDALDataType type )
{
  GDALDataset  *Dataset;
  GDALRasterBand *Band;
  Dataset = static_cast<GDALDataset *>( GDALOpen( fileName, GA_ReadOnly ) );

  if ( Dataset == nullptr )
  {
    return false;
  }

  Band = Dataset->GetRasterBand( 1 );
  reserveMemory( Band->GetXSize(), Band->GetYSize() );

  CPLErr err = Band->RasterIO( GF_Read, 0, 0, mColumnCount, mRowCount, mValues.data(), mColumnCount, mRowCount, type, 0, 0 );
  if ( err )
    return false;
  mNoData = Band->GetNoDataValue();

  GDALClose( static_cast<GDALDatasetH>( Dataset ) );

  return true;
}

template<typename T>
bool ReosRasterMemory<T>::createTiffFile( const char *fileName, GDALDataType type, double *geoTrans, OGRSpatialReference *crs )
{
  GDALDriver *driver = GetGDALDriverManager()->GetDriverByName( "GTiff" );

  if ( !driver )
    return false;

  GDALDataset *dataSet = driver->Create( fileName, mColumnCount, mRowCount, 1, type, nullptr );

  CPLErr err = dataSet->GetRasterBand( 1 )->RasterIO( GF_Write, 0, 0, mColumnCount, mRowCount, mValues.data(), mColumnCount, mRowCount, type, 0, 0 );
  if ( err )
    return false;

  dataSet->GetRasterBand( 1 )->SetNoDataValue( mNoData );

  dataSet->SetGeoTransform( geoTrans );
  if ( crs )
  {
    char *proj_WKT = nullptr;
    crs->exportToWkt( &proj_WKT );
    dataSet->SetProjection( proj_WKT );
    CPLFree( proj_WKT );
  }

  GDALClose( static_cast<GDALDatasetH>( dataSet ) );

  return true;
}

template<typename T>
bool ReosRasterMemory<T>::createTiffFile( const char *fileName, GDALDataType type, const ReosRasterExtent &emprise, OGRSpatialReference *crs )
{
  double geoTrans[6] = {emprise.xMapOrigin(), emprise.xCellSize(), 0, emprise.yMapOrigin(), 0, emprise.yCellSize()};
  return createTiffFile( fileName, type, geoTrans, crs );
}


template<typename T>
bool ReosRasterMemory<T>::copy( ReosRasterMemory<T> *other )
{
  if ( !other )
    return false;

  mRowCount = other->mRowCount;
  mColumnCount = other->mColumnCount;

  try
  {
    mValues = other->mValues;
    return true;
  }
  catch ( std::bad_alloc &e )
  {
    return false;
  }
}

template<typename T>
ReosRasterMemory<T> *ReosRasterMemory<T>::reduceRaster( int rowMin, int rowMax, int columnMin, int columnMax )
{
  if ( ( rowMax < rowMin ) || ( columnMax < columnMin ) )
    return nullptr;
  ReosRasterMemory<T> *returnRaster = new ReosRasterMemory<T>( rowMax - rowMin + 1, columnMax - columnMin + 1 );
  returnRaster->reserveMemory();

  for ( int row = rowMin; row <= rowMax; ++row )
    for ( int col = columnMin; col <= columnMax; ++col )
    {
      returnRaster->setValue( row - rowMin, col - columnMin, value( row, col ) );
    }

  return returnRaster;
}

template<typename T>
bool ReosRasterMemory<T>::operator==( const ReosRasterMemory<T> &rhs ) const
{
  if ( !isValid() || !rhs.isValid() )
    return false;

  if ( mRowCount != rhs.mRowCount || mColumnCount != rhs.mColumnCount )
    return false;

  for ( int i = 0; i < mRowCount; ++i )
    for ( int j = 0; j < mColumnCount; ++j )
    {
      if ( value( i, j ) != rhs.value( i, j ) )
        return false;
    }

  return true;
}



template<typename T>
bool ReosRasterMemory<T>::isValid() const
{
  return !mValues.empty() && mValues.size() == mRowCount * mColumnCount;
}


/**
 * Convenient class used to navigate in a raster and can hande the raser value at corresponding position
 */
template <typename T>
class ReosRasterCellValue: public ReosRasterCellPos
{
  public:

    ReosRasterCellValue( ReosRasterMemory<T> &raster ): ReosRasterCellPos( 0, 0 ), mRaster( raster )
    {}

    ReosRasterCellValue( ReosRasterMemory<T> &raster, int row, int col ): ReosRasterCellPos( row, col ), mRaster( raster )
    {}

    ReosRasterCellValue( const ReosRasterCellValue<T> &other ):
      ReosRasterCellPos( other.row(), other.column() ),
      mRaster( other.mRaster )
    {
    }

    bool operator<( const ReosRasterCellValue<T> &other ) const
    {
      return value() < other.value();
    }

    bool operator<=( const ReosRasterCellValue<T> &other ) const
    {
      return value() <= other.value();
    }

    T value() const {return mRaster.value( row(), column() );}
    void setValue( T value ) {mRaster.setValue( row(), column(), value );}

    bool isBorder() const
    {
      if ( row() == 0 )
        return true;

      if ( row() == mRaster->getRowCount() - 1 )
        return true;

      if ( column() == 0 )
        return true;

      if ( column() == mRaster->getColumnCount() - 1 )
        return true;

      return  false;
    }

    bool isValid() const override
    {

      if ( ! mRaster.isValid() )
        return false;

      if ( row() >= mRaster.rowCount() )
        return false;

      if ( column() >= mRaster.columnCount() )
        return false;

      if ( row() < 0 || column() < 0 )
        return false;

      return ReosRasterCellPos::isValid();
    }

    ReosRasterCellValue &operator=( const ReosRasterCellValue &other )
    {
      mRaster = other.mRaster;
      setColumn( other.column() );
      setRow( other.row() );

      return *this;
    }

  private:
    ReosRasterMemory<T> &mRaster;

};

/**
 * Class that can be used as neighbor circulator arround a raster cell
 */
class RasterNeighborCirculator
{
  public:
    //! Constructor from the central position, the position is as same row row and next column
    RasterNeighborCirculator( const ReosRasterCellPos &mCentral );
    //! Constructor from the central position, the position is defined with \a delta_Row and \a delta_Column that has to be -1, 0 or 1
    RasterNeighborCirculator( const ReosRasterCellPos &mCentral, short delta_Row, short delta_Column );
    //! Turns counter clock wise
    RasterNeighborCirculator &operator++();
    //! Turns clock wise
    RasterNeighborCirculator &operator--();
    //! Returns true if the position in the raster are the same
    bool operator==( const RasterNeighborCirculator &other ) const;
    //! Returns false if the position in the raster are the same
    bool operator!=( const RasterNeighborCirculator &other ) const;
    //! Returns the positon in the raster
    ReosRasterCellPos getPosition() const;
    //! Sets the position is defined with \a delta_Row and \a delta_Column that has to be -1, 0 or 1
    void setRelativePosition( short delta_Row, short delta_Column );

  private:
    const ReosRasterCellPos mCentral;
    unsigned mPos = 0;

    static const std::array<ReosRasterCellPos, 8> cyclePositionToRelativePosition;
    static const std::array<std::array<unsigned, 3>, 3> relativePositionToCyclePosition;

    // -------------
    // | 3 | 2 | 1 |
    // -------------
    // | 4 |   | 0 |
    // -------------
    // | 5 | 6 | 7 |
    // -------------
    //
    // 0 ..7 : cyclePositon
    // i=-1,0,+1 j=-1,0,+1 : relative position
};



class ReosRasterTestingCell
{
  public:
    virtual ~ReosRasterTestingCell() = default;
    virtual bool testCell( const ReosRasterCellPos &cell ) const;
};

class ReosRasterTestingCellInPolygon: public ReosRasterTestingCell
{
  public:
    ReosRasterTestingCellInPolygon( ReosRasterExtent emprise, const QPolygonF &polygon );

    bool testCell( const ReosRasterCellPos &cell ) const override;

  private:
    ReosRasterExtent mExtent;
    const QPolygonF mPolygon;
};


#endif // REOSMEMORYRASTER_H



