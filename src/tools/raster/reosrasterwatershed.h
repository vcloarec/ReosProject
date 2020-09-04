/***************************************************************************
                      hdwatershedfromraster.h
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

#ifndef HDWATERSHEDFROMRASTER_H
#define HDWATERSHEDFROMRASTER_H

#include <queue>
#include <algorithm>

#include "reosprocess.h"
#include "hdrasterdata.h"
#include "hdrasterline.h"
#include "hdrastertools.h"


class HdWatershedFromDirectionAndDownStreamLine;



class HdWatershedFromRasterUniqueThread: public ReosProcess
{
  public:

    struct Climber
    {
      Climber( CellPos p ): pos( p )
      {}
      Climber( CellPos p, double length ): pos( p ), lengthPath( length ) {}
      CellPos pos;
      double lengthPath = 0;
    };

    HdWatershedFromRasterUniqueThread( HdWatershedFromDirectionAndDownStreamLine *parent,
                                       HdWatershedFromRasterUniqueThread::Climber initialClimb,
                                       std::shared_ptr<RasterMemoire<unsigned char>> directionRaster,
                                       std::shared_ptr<RasterMemoire<unsigned char>> resultRaster,
                                       std::shared_ptr<HdRasterLineInterface> excludedPixel );




  private:
    HdWatershedFromDirectionAndDownStreamLine *parent;
    std::shared_ptr<RasterMemoire<unsigned char>> directionRaster;
    std::shared_ptr<RasterMemoire<unsigned char>> resultRaster;
    Climber climber;
    std::shared_ptr<HdRasterLineInterface> excludedPixel;
    std::queue<Climber> climbToTreat;

    // Process interface
  public:
    void start();

};


class HdWatershedFromRaster: public ReosProcess
{
  public:
    HdWatershedFromRaster() {}

    virtual ~HdWatershedFromRaster() {}


};

class HdTestingPixel
{
  public:
    virtual ~HdTestingPixel()  {}
    virtual bool testPixel( const CellPos &px ) const
    {
      Q_UNUSED( px );
      return true;
    }
};

class HdTestingPixelInMapPolygon: public HdTestingPixel
{
  public:

    HdTestingPixelInMapPolygon( ReosRasterExtent emprise, const QPolygonF &polygon ): emprise( emprise ), polygon( polygon ) {}

  private:
    ReosRasterExtent emprise;
    const QPolygonF &polygon;


    // HdTestingPixel interface
  public:
    bool testPixel( const CellPos &px ) const override
    {
      QPointF pointMap = emprise.pixelCenterToMap( px );
      return polygon.containsPoint( pointMap, Qt::OddEvenFill );
    }
};


class HdWatershedFromDirectionAndDownStreamLine: public HdWatershedFromRaster
{
  public:
    HdWatershedFromDirectionAndDownStreamLine( std::shared_ptr<RasterMemoire<unsigned char>> rasterDirection, std::shared_ptr<HdRasterLineInterface> line );
    HdWatershedFromDirectionAndDownStreamLine( std::shared_ptr<RasterMemoire<unsigned char>> rasterDirection, std::shared_ptr<HdRasterLineInterface> line, HdTestingPixel *pixelTester );
    ~HdWatershedFromDirectionAndDownStreamLine();

    HdWatershedFromRasterUniqueThread::Climber getClimberFromPool( bool &available );
    void proposeEndOfPath( HdWatershedFromRasterUniqueThread::Climber climber );

    std::shared_ptr<RasterMemoire<unsigned char>> getWatershed() const;

    CellPos getFisrtPixel() const {return firstPixel;}
    CellPos getEndOfGreaterPath() const {return endOfGreaterPath.pos;}

    bool testPixel( const CellPos &px )
    {
      if ( pixelTester )
        return pixelTester->testPixel( px );
      else
        return true;
    }

  private:
    std::shared_ptr<RasterMemoire<unsigned char>> direction;
    std::shared_ptr<RasterMemoire<unsigned char>> watershed;
    std::shared_ptr<HdRasterLineInterface> downstreamLine;
    std::list<HdWatershedFromRasterUniqueThread::Climber> poolInitialPixelsToTreat;
    int counter;
    std::vector<std::thread> threads;
    std::vector<HdWatershedFromRasterUniqueThread *> calculateObjects;
    CellPos firstPixel;

    HdWatershedFromRasterUniqueThread::Climber endOfGreaterPath;

    std::mutex mutexPixel;
    std::mutex mutexEndOfPath;

    const HdTestingPixel *pixelTester = nullptr;

    // Process interface
  public:
    void start();

    virtual void setStopWithMutex( bool b );
};

class HdWatershedPolygonFromWatershedRaster: public Process
{
  public:

    HdWatershedPolygonFromWatershedRaster( std::shared_ptr<RasterMemoire<unsigned char>> rasterWatershed, ReosRasterExtent emprise, CellPos pixelInWaterShed );

    const QPolygon &getWatershedDelineate() const;

  private:
    std::shared_ptr<RasterMemoire<unsigned char>> rasterWatershed;
    ReosRasterExtent emprise;

    std::unique_ptr<traceurInterPixelValeurIdentique<unsigned char>> traceur = nullptr;

    QList<QPoint> *elim = nullptr;

    // Process interface
  public:
    void start();

};

class HdDrawGoDownTrace: public Process
{
  public:
    HdDrawGoDownTrace( std::shared_ptr<RasterMemoire<unsigned char>> directionRaster,
                       std::shared_ptr<HdRasterLineInterface> stopLine,
                       ReosRasterExtent empriseRaster, CellPos startPos ):
      directionRaster( directionRaster ),
      stopLine( stopLine ),
      empriseRaster( empriseRaster ),
      pos( startPos )
    {}

    HdDrawGoDownTrace( std::shared_ptr<RasterMemoire<unsigned char>> directionRaster,
                       QPolygonF polyLimit,
                       ReosRasterExtent empriseRaster, CellPos startPos ):
      directionRaster( directionRaster ),
      empriseRaster( empriseRaster ),
      pos( startPos ), polyLimit( polyLimit )
    {}

    QPolygonF getResultPolyline() const
    {
      return resultPolyline;
    }
  private:

    std::shared_ptr<RasterMemoire<unsigned char>> directionRaster;
    std::shared_ptr<HdRasterLineInterface> stopLine;
    ReosRasterExtent empriseRaster;
    CellPos pos;

    QPolygonF resultPolyline;
    QPolygonF polyLimit;

    // Process interface
  public:
    void start() override
    {
      unsigned char lastDir = 4;
      unsigned char dir = directionRaster->getValeur( pos.getRow(), pos.getColumn() );
      QPointF posMap = empriseRaster.pixelCenterToMap( pos );
      bool pointIsInPolyLimit = true;
      bool isStopLine = false;
      bool testIsInPolygon = ( polyLimit != QPolygonF() );

      while ( ( !isStopLine ) && ( dir != 4 ) && ( dir != 9 ) && ( !stopWithMutex() ) && pointIsInPolyLimit )
      {
        if ( dir != lastDir )
          resultPolyline.append( posMap );
        lastDir = dir;
        pos = pos.getNeighbourWithDirection( dir );
        posMap = empriseRaster.pixelCenterToMap( pos );

        if ( testIsInPolygon )
          pointIsInPolyLimit = polyLimit.containsPoint( posMap, Qt::OddEvenFill );
        if ( stopLine )
          isStopLine = stopLine->contain( &pos );
        dir = directionRaster->getValeur( pos.getRow(), pos.getColumn() );
      }
      resultPolyline.append( empriseRaster.pixelCenterToMap( pos ) );
    }
};

#endif // HDWATERSHEDFROMRASTER_H
