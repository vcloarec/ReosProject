/***************************************************************************
                      reosrasterline.h
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

#ifndef REOSRASTERLINE_H
#define REOSRASTERLINE_H

#include "reosmemoryraster.h"

class ReosRasterLine
{
  public:
    /**
     * Constructor with \a thick argument that defined the arragment of the cells :
     *
     * thick == false        thick == true
     *   o                      oo
     *    o                      oo
     *     o                      oo
     *      o                      oo
     */
    ReosRasterLine( bool thick = true );

    //! Adds a point in the line at \a row and \a column
    void addPoint( int row, int column );
    //! Adds a point in the line at \a cellPos position
    void addPoint( const ReosRasterCellPos &cellPos );

    //! Returns the last cell position on the line
    virtual const ReosRasterCellPos lastCellPosition() const;
    //! Returns the ith cell position of the line
    virtual const ReosRasterCellPos cellPosition( unsigned i ) const;
    //! Returns the cell count in the line
    virtual unsigned cellCount() const;

    //! Returns whether the line contain the cell position \a cellPos
    bool contains( const ReosRasterCellPos &cellPos );

  private:
    bool mThick;
    std::vector<ReosRasterCellPos> mCells;
    int mRowMin = std::numeric_limits<int>::max();
    int mRowMax = 0;
    int mColMin = std::numeric_limits<int>::max();;
    int mColMax = 0;

    void addCell( int row, int column );
    virtual bool vectorContain( int row, int col ) const;

    void drawLine( int ri, int ci, int rf, int cf );
};

#endif // REOSRASTERLINE_H
