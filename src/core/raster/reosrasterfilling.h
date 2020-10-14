/***************************************************************************
                      reosrasterfilling.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSRASTERFILLING_H
#define REOSRASTERFILLING_H

#include <set>
#include <mutex>
#include <deque>

#include "reosmemoryraster.h"
#include "reosprocess.h"


class ReosRasterFilling: public ReosProcess
{
  public:
    //! Constructor with the \a dem to fill
    ReosRasterFilling( const ReosRasterMemory<float> &dem, double XSize, double YSize ):
      mDem( dem ), mXSize( XSize ), mYSize( YSize )
    {}
    virtual ~ReosRasterFilling();

    virtual bool initialize() = 0;

    //! Returns the minimum slope used as parameter of the filling process
    float mimimumSlope() const;
    //! Sets the minimum slope used as parameter of the filling process
    void setMimimumSlope( float value );

    //! Set the size of the raster cell in the X direction (used to apply slope criteria)
    void setXSize( float value );
    //! Set the size of the raster cell in the Y direction (used to apply slope criteria)
    void setYSize( float value );

    //! Returns the calculated filled DEM, the caller has to take the ownership
    ReosRasterMemory<float> filledDEM();

  protected:

    ReosRasterMemory<float> mDem;
    float mMimimumSlope = 0.0001;
    double mXSize = 0;
    double mYSize = 0;

    unsigned char calculateDirection( int row, int column );
};


class ReosRasterFillingWangLiu: public ReosRasterFilling
{
  public:
    ReosRasterFillingWangLiu( const ReosRasterMemory<float> &dem, double XSize, double YSize );
    ~ReosRasterFillingWangLiu() = default;

    bool initialize();

    void start() override;

    //! Returns the direction raster used to filled the DEM, the caller take ownership
    ReosRasterMemory<unsigned char> directionRaster() {return mRasterChar;}

  private:
    std::multiset<ReosRasterCellValue<float>> mPriorityStack;
    std::deque<ReosRasterCellValue<float>> mNoDataStack;
    std::deque<ReosRasterCellValue<float>> mCelllsToCalculateDirectionAtTheEnd;

    //! used during the process to store which pixel is treated and may be used after for the direction raster
    //! 255 : not treated ; 20X in the priorityStack and X is the direction; 00X : X final direction completly treated                    */
    ReosRasterMemory<unsigned char> mRasterChar;

    bool makePriorityStack();

    void processCell( const ReosRasterCellValue<float> &central );
    void processNoDataCell( const ReosRasterCellValue<float> &central );
    bool calculateBorderDirections();

    void setDirectionValue( int row, int column, unsigned char value );
    int mProgession = 0;
};

#endif // REOSRASTERFILLING_H
