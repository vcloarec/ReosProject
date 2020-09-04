/***************************************************************************
                      hdwatershedfromraster.cpp
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

#include "reosrasterwatershed.h"



HdWatershedFromRasterUniqueThread::HdWatershedFromRasterUniqueThread( HdWatershedFromDirectionAndDownStreamLine *parent, HdWatershedFromRasterUniqueThread::Climber initialClimb, std::shared_ptr<RasterMemoire<unsigned char> > directionRaster, std::shared_ptr<RasterMemoire<unsigned char> > resultRaster, std::shared_ptr<HdRasterLineInterface> excludedPixel ):
  parent( parent ), directionRaster( directionRaster ), resultRaster( resultRaster ), climber( initialClimb ), excludedPixel( excludedPixel )
{
  climbToTreat.push( initialClimb );
}

void HdWatershedFromRasterUniqueThread::start()
{
  while ( ( !climbToTreat.empty() ) && ( !stop() ) )
  {
    Climber currentClimb = climbToTreat.front();
    climbToTreat.pop();

    bool endOfPath = true;

    for ( int i = 0; i < 3; ++i )
      for ( int j = 0; j < 3; ++j )
      {
        if ( ( i != 1 ) || ( j != 1 ) )
        {
          CellPos pixelToTest( currentClimb.pos.getRow() + i - 1, currentClimb.pos.getColumn() + j - 1 );

          unsigned char direction = directionRaster->getValeur( pixelToTest.getRow(), pixelToTest.getColumn() );

          if ( direction == ( 8 - ( i + j * 3 ) ) )
          {
            if ( !excludedPixel->contain( &pixelToTest ) && parent->testPixel( pixelToTest ) )
            {
              resultRaster->setValeur( pixelToTest.getRow(), pixelToTest.getColumn(), 1 );
              double dl = 0;
              if ( direction % 2 == 0 )
              {
                dl = sqrt( 2 );
              }
              else
                dl = 1;

              climbToTreat.push( Climber( pixelToTest, currentClimb.lengthPath + dl ) );

              endOfPath &= false;
            }
          }
        }
      }

    if ( endOfPath )
    {
      parent->proposeEndOfPath( currentClimb );
    }

    if ( climbToTreat.empty() )
    {
      bool pixelAvailable;
      Climber pix = parent->getClimberFromPool( pixelAvailable );
      if ( pixelAvailable )
        climbToTreat.push( pix );
    };

    if ( stopWithMutex() )
      setStop( true );

  }


}

HdWatershedFromDirectionAndDownStreamLine::HdWatershedFromDirectionAndDownStreamLine( std::shared_ptr<RasterMemoire<unsigned char> > rasterDirection, std::shared_ptr<HdRasterLineInterface> line ):
  direction( rasterDirection ), downstreamLine( line )
{
  watershed = std::shared_ptr<RasterMemoire<unsigned char>>( new RasterMemoire<unsigned char>( direction->getRowCount(), direction->getColumnCount() ) );
  watershed->alloueMemoire();
  watershed->rempli( 0 );



  for ( unsigned i = 0; i < downstreamLine->getPixelPositionsNumber(); ++i )
  {
    CellPos pix = *downstreamLine->getPosition( i );
    poolInitialPixelsToTreat.push_back( HdWatershedFromRasterUniqueThread::Climber( pix ) );
    watershed->setValeur( pix.getRow(), pix.getColumn(), 1 );

  }

  setMaxProgession( int( poolInitialPixelsToTreat.size() ) );
  //firstPixel=poolInitialPixelsToTreat.front();
  unsigned halfPos = downstreamLine->getPixelPositionsNumber() / 2;
  firstPixel = *downstreamLine->getPosition( halfPos );

}

HdWatershedFromDirectionAndDownStreamLine::HdWatershedFromDirectionAndDownStreamLine( std::shared_ptr<RasterMemoire<unsigned char> > rasterDirection, std::shared_ptr<HdRasterLineInterface> line, HdTestingPixel *pixelTester_ ):
  HdWatershedFromDirectionAndDownStreamLine( rasterDirection, line )

{
  pixelTester = pixelTester_;
}

HdWatershedFromDirectionAndDownStreamLine::~HdWatershedFromDirectionAndDownStreamLine()
{
  if ( pixelTester )
    delete pixelTester;
}


HdWatershedFromRasterUniqueThread::Climber HdWatershedFromDirectionAndDownStreamLine::getClimberFromPool( bool &available )
{
  HdWatershedFromRasterUniqueThread::Climber climber;
  mutexPixel.lock();
  if ( poolInitialPixelsToTreat.empty() )
  {
    available = false;
  }
  else
  {
    available = true;
    climber = poolInitialPixelsToTreat.front();
    poolInitialPixelsToTreat.pop_front();
    counter++;
    setCurrentProgression( counter );
  }
  mutexPixel.unlock();
  return climber;
}

void HdWatershedFromDirectionAndDownStreamLine::proposeEndOfPath( HdWatershedFromRasterUniqueThread::Climber climber )
{
  mutexEndOfPath.lock();
  if ( climber.lengthPath > endOfGreaterPath.lengthPath )
  {
    endOfGreaterPath = climber;
  }
  mutexEndOfPath.unlock();
}

std::shared_ptr<RasterMemoire<unsigned char> > HdWatershedFromDirectionAndDownStreamLine::getWatershed() const {return watershed;}

void HdWatershedFromDirectionAndDownStreamLine::start()
{

  unsigned nbThread = std::thread::hardware_concurrency() - 1;


  threads.clear();
  calculateObjects.clear();

  for ( unsigned i = 0; i < nbThread; ++i )
  {
    bool pixelAvailable;
    HdWatershedFromRasterUniqueThread::Climber pix = getClimberFromPool( pixelAvailable );
    if ( pixelAvailable )
    {
      HdWatershedFromRasterUniqueThread *cal = new HdWatershedFromRasterUniqueThread( this, pix, direction, watershed, downstreamLine );
      calculateObjects.push_back( cal );
      threads.emplace_back( processStart, cal );
    }
  }

  for ( auto &&t : threads )
  {
    t.join();
  }

  for ( auto &&calc : calculateObjects )
    delete calc;

  calculateObjects.clear();
  threads.clear();
}

void HdWatershedFromDirectionAndDownStreamLine::setStopWithMutex( bool b )
{
  for ( auto calc : calculateObjects )
    calc->setStopWithMutex( b );
}

HdWatershedPolygonFromWatershedRaster::HdWatershedPolygonFromWatershedRaster( std::shared_ptr<RasterMemoire<unsigned char> > rasterWatershed, ReosRasterExtent emprise, CellPos pixelInWaterShed ):
  rasterWatershed( rasterWatershed ), emprise( emprise )
{
  bool findLimit = false;
  int Columnlimite = pixelInWaterShed.getColumn();

  while ( ( !findLimit ) && ( Columnlimite > -1 ) )
  {
    Columnlimite--;
    if ( rasterWatershed->getValeur( pixelInWaterShed.getRow(), Columnlimite ) != 1 )
    {
      findLimit = true;
    }
  }


  Columnlimite++;

  QPoint interPixelDepart = QPoint( pixelInWaterShed.getRow(), Columnlimite );
  QPoint origine( -1, 0 );

  QVector<QPoint> ligneArrivee;
  ligneArrivee.append( interPixelDepart ); //ici, l'interPIxel est Ã©quivalent au pixel car l'interpixel est ici situÃ© en haut Ã  gauche du point du pixel.

  elim = new QList<QPoint>();
  traceur = std::unique_ptr<traceurInterPixelValeurIdentique<unsigned char>>( new traceurInterPixelValeurIdentique<unsigned char>( rasterWatershed, 1, interPixelDepart, origine, ligneArrivee, elim, 20000000 ) );

  setMaxProgession( 0 );
}

const QPolygon &HdWatershedPolygonFromWatershedRaster::getWatershedDelineate() const {return traceur->getTrace();}

void HdWatershedPolygonFromWatershedRaster::start()
{
  traceur->trace();
  delete elim;
}
