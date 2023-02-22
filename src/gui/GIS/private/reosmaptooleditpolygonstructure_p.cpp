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
#include<QMenu>

ReosMapToolEditPolygonStructure_p::ReosMapToolEditPolygonStructure_p( QgsMapCanvas *mapCanvas )
  : ReosMapTool_p( mapCanvas )
{
  enableSnapping( true );
  mPolygonRubberBand = new QgsRubberBand( mCanvas, Qgis::GeometryType::Polygon );
  mPolygonRubberBand->setWidth( 1 );
  mPolygonRubberBand->setLineStyle( Qt::DashLine );
  mPolygonRubberBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos() );
  mPolygonRubberBand->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mPolygonRubberBand->setSecondaryStrokeColor( Qt::white );
  mPolygonRubberBand->setZValue( 50 );

  mMainActionsGroup = new QActionGroup( this );

}

QgsMapTool::Flags ReosMapToolEditPolygonStructure_p::flags() const
{
  switch ( mCurrentState )
  {
    case AddingPolygon:
      break;
    case None:
      if ( hasFeatureOnMap( mCurrentPosition ) )
        return Flags();
      if ( !hasHelperPolygon() )
        return Flags();
      return ShowContextMenu;
      break;
  }

  return Flags();
}

void ReosMapToolEditPolygonStructure_p::setStructure( ReosPolygonStructure *structure )
{
  mStructure = structure;

  mActionUndo = structure->undoStack()->createUndoAction( this );
  mActionUndo->setIcon( QIcon( QStringLiteral( ":/images/undoOrange.svg" ) ) );
  mActionRedo = structure->undoStack()->createRedoAction( this );
  mActionRedo->setIcon( QIcon( QStringLiteral( ":/images/redoOrange.svg" ) ) );

  mMainActionsGroup->addAction( mActionUndo );
  mMainActionsGroup->addAction( mActionRedo );
}

QActionGroup *ReosMapToolEditPolygonStructure_p::mainActions() const
{
  return mMainActionsGroup;
}

void ReosMapToolEditPolygonStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  mCurrentPosition = e->mapPoint().toQPointF();

  const QPointF &snapPoint = e->snapPoint().toQPointF();
  mPolygonRubberBand->movePoint( snapPoint );

  ReosMapTool_p::canvasMoveEvent( e );
}

void ReosMapToolEditPolygonStructure_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  const QPointF &snapPoint = e->snapPoint().toQPointF();

  switch ( mCurrentState )
  {
    case ReosMapToolEditPolygonStructure_p::None:
      if ( e->button() == Qt::LeftButton )
      {
        mCurrentState = AddingPolygon;
        mPolygonRubberBand->addPoint( snapPoint );
      }
      else if ( e->button() == Qt::RightButton )
      {
        QgsGeometry geom = selectFeatureOnMap( e );
        if ( !geom.isNull() && !geom.isEmpty() )
          mStructure->addPolygon( geom.asQPolygonF(), mCurrentClassId, mapCrs() );
      }
      break;
    case ReosMapToolEditPolygonStructure_p::AddingPolygon:
      if ( e->button() == Qt::LeftButton )
        mPolygonRubberBand->addPoint( snapPoint );
      else if ( e->button() == Qt::RightButton )
      {
        if ( mPolygonRubberBand->numberOfVertices() > 0 )
        {
          const QPolygonF polygon = mPolygonRubberBand->asGeometry().asQPolygonF();
          mStructure->addPolygon( polygon, mCurrentClassId, mapCrs() );
        }
        resetTool();
      }
      break;
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

void ReosMapToolEditPolygonStructure_p::addHelperStructure( ReosGeometryStructure *structure )
{
  mHelperStructure.append( structure );
}

void ReosMapToolEditPolygonStructure_p::activate()
{
  mMainActionsGroup->setEnabled( true );
  ReosMapTool_p::activate();
}

void ReosMapToolEditPolygonStructure_p::deactivate()
{
  mMainActionsGroup->setEnabled( false );
  ReosMapTool_p::deactivate();
}

void ReosMapToolEditPolygonStructure_p::resetTool()
{
  mCurrentState = None;
  mPolygonRubberBand->reset( Qgis::GeometryType::Polygon );
}

void ReosMapToolEditPolygonStructure_p::addPolygon( const QPolygonF &polygon )
{

}

bool ReosMapToolEditPolygonStructure_p::hasHelperPolygon() const
{
  ReosSpatialPosition position( mCurrentPosition, mapCrs() );
  for ( const QPointer<ReosGeometryStructure> &str : std::as_const( mHelperStructure ) )
  {
    if ( str.isNull() )
      continue;
    if ( !str->searchPolygon( position, true ).isEmpty() )
      return true;
  }
  return false;
}

ReosEditPolygonStructureMenuPopulator::ReosEditPolygonStructureMenuPopulator( ReosMapToolEditPolygonStructure_p *toolMap )
  : mToolMap( toolMap )
{}


bool ReosEditPolygonStructureMenuPopulator::populate( QMenu *menu, QgsMapMouseEvent *e )
{
  QPolygonF structurePolygon;
  ReosSpatialPosition position( e->mapPoint().toQPointF(), mToolMap->mapCrs() );
  for ( const QPointer<ReosGeometryStructure> &str : std::as_const( mToolMap->mHelperStructure ) )
  {
    if ( str.isNull() )
      continue;
    structurePolygon = str->searchPolygon( position, true );
    if ( !structurePolygon.isEmpty() )
      break;
  }


  if ( !structurePolygon.isEmpty() )
  {
    menu->clear();
    QString stringValue;
    double value = mToolMap->mStructure->value( mToolMap->mCurrentClassId );
    if ( std::isnan( value ) )
      stringValue = QObject::tr( "default" );
    else
      stringValue = QLocale().toString( value );

    QAction *actionStructurePoly = menu->addAction( QObject::tr( "Apply %1 to this polygon" ).arg( stringValue ) );

    QgsRubberBand *polyRubberBand = new QgsRubberBand( mToolMap->mCanvas );
    polyRubberBand->reset( Qgis::GeometryType::Polygon );
    QPolygonF polyForRubber = structurePolygon;
    polyForRubber.append( polyForRubber.first() );
    polyRubberBand->setToGeometry( QgsGeometry::fromQPolygonF( polyForRubber ) );
    polyRubberBand->setVisible( false );
    polyRubberBand->setColor( ReosStyleRegistery::instance()->blueReos( 100 ) );

    QObject::connect( menu, &QMenu::hovered, polyRubberBand, [polyRubberBand]
    {
      polyRubberBand->setVisible( false );
    } );

    QObject::connect( actionStructurePoly, &QAction::hovered, polyRubberBand, [polyRubberBand]
    {
      polyRubberBand->setVisible( true );
    } );

    QObject::connect( menu, &QObject::destroyed, polyRubberBand, [polyRubberBand]
    {polyRubberBand->deleteLater();} );

    QObject::connect( actionStructurePoly, &QAction::triggered, mToolMap, [this, structurePolygon]
    {
      mToolMap->mStructure->addPolygon( structurePolygon, mToolMap->mCurrentClassId, mToolMap->mapCrs() );
    } );
    return true;
  }

  return false;
}
