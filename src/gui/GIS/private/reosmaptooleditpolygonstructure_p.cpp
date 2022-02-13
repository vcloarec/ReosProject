/***************************************************************************
  reosmaptooleditpolygonstructure_p.cpp - ReosMapToolEditPolygonStructure_p

 ---------------------
 begin                : 6.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmaptooleditpolygonstructure_p.h"
#include "reosstyleregistery.h"

#include<QUuid>

ReosMapToolEditPolygonStructure_p::ReosMapToolEditPolygonStructure_p( QgsMapCanvas *mapCanvas )
  : ReosMapTool_p( mapCanvas )
{
  enableSnapping( true );
  mPolygonRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::PolygonGeometry );
  mPolygonRubberBand->setWidth( 1 );
  mPolygonRubberBand->setLineStyle( Qt::DashLine );
  mPolygonRubberBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos() );
  mPolygonRubberBand->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mPolygonRubberBand->setSecondaryStrokeColor( Qt::white );
  mPolygonRubberBand->setZValue( 50 );

  mMainActionsGroup = new QActionGroup( this );

}

void ReosMapToolEditPolygonStructure_p::setStructure( ReosPolygonStructure *structure )
{
  mStructure = structure;

  mActionUndo = structure->undoStack()->createUndoAction( this );
  mActionUndo->setIcon( QPixmap( QStringLiteral( ":/images/undoOrange.svg" ) ) );
  mActionRedo = structure->undoStack()->createRedoAction( this );
  mActionRedo->setIcon( QPixmap( QStringLiteral( ":/images/redoOrange.svg" ) ) );

  mMainActionsGroup->addAction( mActionUndo );
  mMainActionsGroup->addAction( mActionRedo );
}

QActionGroup *ReosMapToolEditPolygonStructure_p::mainActions() const
{
  return mMainActionsGroup;
}

void ReosMapToolEditPolygonStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  const QPointF &snapPoint = e->snapPoint().toQPointF();
  mPolygonRubberBand->movePoint( snapPoint );

  ReosMapTool_p::canvasMoveEvent( e );
}

void ReosMapToolEditPolygonStructure_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  const QPointF &snapPoint = e->snapPoint().toQPointF();

  if ( e->button() == Qt::LeftButton )
  {
    mPolygonRubberBand->addPoint( snapPoint );
  }
  else if ( e->button() == Qt::RightButton )
  {
    const QPolygonF polygon = mPolygonRubberBand->asGeometry().asQPolygonF();
    mStructure->addPolygon( polygon, mCurrentClassId );
    resetTool();
  }
}

void ReosMapToolEditPolygonStructure_p::keyPressEvent( QKeyEvent *e )
{
  if ( e->key() == Qt::Key_Escape )
    resetTool();
  else
    ReosMapTool_p::keyPressEvent( e );
}

void ReosMapToolEditPolygonStructure_p::setCurrentClassId( const QString &currentClassId )
{
  mCurrentClassId = currentClassId;
}

void ReosMapToolEditPolygonStructure_p::resetTool()
{
  mPolygonRubberBand->reset( QgsWkbTypes::PolygonGeometry );
}

void ReosMapToolEditPolygonStructure_p::addPolygon( const QPolygonF &polygon )
{

}
