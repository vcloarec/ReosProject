/***************************************************************************
                      reosrasterfilling.h
                     --------------------------------------
Date                 : 18-11-2018
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
    ReosRasterFilling( const ReosRasterMemory<float> &dem, double XSize, double YSize, float maxValue );
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

    //! Returns the calculated filled DEM
    const ReosRasterMemory<float> &filledDEM() const;

  protected:

    ReosRasterMemory<float> mDem;
    float mMimimumSlope = 0.0001f;
    double mXSize = 0;
    double mYSize = 0;

};


class ReosRasterFillingWangLiu: public ReosRasterFilling
{
  public:
    ReosRasterFillingWangLiu( const ReosRasterMemory<float> &dem, double XSize, double YSize, float maxValue );
    ~ReosRasterFillingWangLiu() = default;

    bool initialize() override;

    void start() override;

  private:
    std::multiset<ReosRasterCellValue<float>> mPriorityStack;
    std::deque<ReosRasterCellValue<float>> mNoDataStack;

    //! used during the process to store which pixel is treated
    //! 255 : not treated ; 254 in the priorityStack and X is the direction; 00X : X final direction completly treated                    */
    ReosRasterMemory<bool> mRasterChar;

    bool makePriorityStack();

    void processCell( const ReosRasterCellValue<float> &central );
    void processNoDataCell( const ReosRasterCellValue<float> &central );

    void markPixel( int row, int column );
    int mProgession = 0;
};

#endif // REOSRASTERFILLING_H
