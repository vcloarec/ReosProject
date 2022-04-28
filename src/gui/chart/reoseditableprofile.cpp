/***************************************************************************
  reoseditableprofile.cpp - ReosEditableProfile

 ---------------------
 begin                : 14.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoseditableprofile.h"

#include "reosprofileplot_p.h"
#include "reosplotpicker_p.h"

#include <QMenu>
#include <QPixmap>


ReosEditableProfile::ReosEditableProfile():
  ReosPlotItem()
  , mActionEditProfile( new QAction( QPixmap( ":/images/editProfile.svg" ), tr( "Edit Profile" ), this ) )
  , mActionCreateNewProfile( new QAction( QPixmap( ":/images/drawProfile.svg" ), tr( "Create New Profile" ), this ) )
  , mActionGroup( new QActionGroup( this ) )
  , mActionZoomExtent( new QAction( QPixmap( ":/images/profileExtent.svg" ), tr( "Zoom on Defined Profile Extent" ), this ) )
  , mActionRemovePoint( new QAction( QPixmap( ":/images/profileRemovePoint.svg" ), tr( "Remove Point" ), this ) )
  , mActionInsertPoint( new QAction( QPixmap( ":/images/profileAddPoint.svg" ), tr( "Insert point" ), this ) )


{
  mPlotItem = new ReosProfilePlot_p( mModel.points() );
  connect( &mModel, &ReosEditableProfileModel::pointChanged, this, &ReosEditableProfile::profileChanged );
  connect( &mModel, &ReosEditableProfileModel::pointInserted, this, &ReosEditableProfile::profileChanged );
  connect( &mModel, &ReosEditableProfileModel::pointRemoved, this, &ReosEditableProfile::profileChanged );
  connect( &mModel, &ReosEditableProfileModel::pointChanged, this, &ReosEditableProfile::itemChanged );
  connect( &mModel, &ReosEditableProfileModel::pointInserted, this, &ReosEditableProfile::itemChanged );
  connect( &mModel, &ReosEditableProfileModel::pointRemoved, this, &ReosEditableProfile::itemChanged );

  mActionEditProfile->setCheckable( true );
  mActionCreateNewProfile->setCheckable( true );
  mActionGroup->addAction( mActionCreateNewProfile );
  mActionGroup->addAction( mActionEditProfile );
  mActionGroup->setExclusive( true );

  connect( mActionRemovePoint, &QAction::triggered, this, &ReosEditableProfile::deletePoint );
  connect( mActionInsertPoint, &QAction::triggered, this, &ReosEditableProfile::insertPoint );
  connect( mActionZoomExtent, &QAction::triggered, this, &ReosEditableProfile::zoomExtent );
}

void ReosEditableProfile::attach( ReosPlot_p *plot )
{
  mPlot = plot;
  ReosPlotItem::attach( plot );

  mPickerEditPoint = new ReosPlotPickerEditPoint_p( plot );
  connect( mPickerEditPoint, &ReosPlotPickerEditPoint_p::rightClick, this, &ReosEditableProfile::contextMenuEdition );
  connect( mPickerEditPoint, &ReosPlotPicker_p::activated, this, &ReosEditableProfile::pickerActivated );
  connect( mPickerEditPoint, &ReosPlotPicker_p::deactivated, this, &ReosEditableProfile::activateZoomer );
  connect( mPickerEditPoint, &ReosPlotPicker_p::deactivated, mPlot, [this] {mPlot->enableAutoScale( true );} );
  connect( mPickerEditPoint, &ReosPlotPickerEditPoint_p::purposeBeginMove, this, &ReosEditableProfile::beginMove );
  connect( mPickerEditPoint, &ReosPlotPickerEditPoint_p::moved, this, &ReosEditableProfile::movePoint );
  connect( mPickerEditPoint, &ReosPlotPickerEditPoint_p::moveFinished, this, &ReosEditableProfile::endMovePoint );
  mPickerEditPoint->setEnabled( false );

  mPickerNewProfile = new ReosPlotPickerDrawLines_p( plot );
  connect( mPickerNewProfile, &ReosPlotPicker_p::activated, this, &ReosEditableProfile::pickerActivated );
  connect( mPickerNewProfile, &ReosPlotPicker_p::deactivated, this, &ReosEditableProfile::activateZoomer );
  connect( mPickerNewProfile, &ReosPlotPicker_p::deactivated, mPlot, [this] {mPlot->enableAutoScale( true );} );
  connect( mPickerNewProfile, &ReosPlotPicker_p::appended, this, &ReosEditableProfile::createProfileNewPoint );
  //connect( mPickerNewProfile, &QwtPlotPicker::selected, this, &ReosEditableProfile::finishProfile ); //don't know why (not investigate a lot) but do not work
  connect( mPickerNewProfile, SIGNAL( selected( const QVector< QPointF > ) ), this, SLOT( finishProfile( const QVector< QPointF > ) ) ); //so instead use the "old way"
  mPickerNewProfile->setEnabled( false );

  QPen penRubber;
  penRubber = QPen( QColor( 0, 0, 0, 150 ) );
  penRubber.setWidth( 3 );
  penRubber.setStyle( Qt::DashLine );
  mPickerNewProfile->setRubberBandPen( penRubber );

  mPickerEditPoint->setAction( mActionEditProfile );
  mPickerNewProfile->setAction( mActionCreateNewProfile );

}

QAbstractTableModel *ReosEditableProfile::tableModel()
{
  return &mModel;
}

QList<QAction *> ReosEditableProfile::actionsToolBar() const
{
  QList<QAction *> ret = mActionGroup->actions();
  ret.append( mActionZoomExtent );
  return ret;
}

QPolygonF ReosEditableProfile::profile() const
{
  return mModel.points();
}

void ReosEditableProfile::beginMove( const QRectF &rect )
{
  mMovingPointIndex = mModel.pointIndexIn( rect );
}

void ReosEditableProfile::movePoint( const QPointF &newPos )
{
  if ( mMovingPointIndex >= 0 )
  {
    mModel.movePoint( mMovingPointIndex, newPos );
    emit itemChanged();
  }
}

void ReosEditableProfile::endMovePoint()
{
  mMovingPointIndex = -1;
  emit itemChanged();
  emit profileChanged();
}

void ReosEditableProfile::createProfileNewPoint( const QPointF &newPos )
{
  if ( mWaitingForFirstPoint )
  {
    mOldProfile = mModel.points();
    mModel.clear();
    mWaitingForFirstPoint = false;
  }
  else
  {
    mModel.addPoint( newPos );
    mPlot->replot();
    emit itemChanged();
  }
}

void ReosEditableProfile::finishProfile( const QVector<QPointF> &pts )
{
  Q_UNUSED( pts )
  mWaitingForFirstPoint = true;
  if ( mModel.pointCount() < 2 )
  {
    mModel.setPoints( mOldProfile );
  }
  emit itemChanged();

  mPickerNewProfile->deactivate();

}

void ReosEditableProfile::pickerActivated( ReosPlotPicker_p *picker )
{
  if ( mCurrentPicker != picker )
  {
    if ( mCurrentPicker )
      mCurrentPicker->deactivate();
    mCurrentPicker = picker;
  }
  deactivateZoomer();
  mPlot->enableAutoScale( false );
}

void ReosEditableProfile::deactivateZoomer()
{
  if ( mPlot )
    mPlot->setEnableZoomer( false );
}

void ReosEditableProfile::activateZoomer()
{
  if ( mPlot )
    mPlot->setEnableZoomer( true );
}

void ReosEditableProfile::contextMenuEdition( const QRectF &rect )
{
  mContextMenuTargetPointIndex = mModel.pointIndexIn( rect );
  mContextMenuPoint = rect.topLeft() + QPointF( rect.width() / 2, rect.height() / 2 );
  QMenu contextMenu;
  if ( mContextMenuTargetPointIndex >= 0 )
    contextMenu.addAction( mActionRemovePoint );
  contextMenu.addAction( mActionInsertPoint );

  QPointF pos = QwtScaleMap::transform( mPlot->canvasMap( QwtPlot::xBottom ), mPlot->canvasMap( QwtPlot::yLeft ), mContextMenuPoint );

  contextMenu.exec( mPlot->canvas()->mapToGlobal( pos.toPoint() ) );

  mContextMenuTargetPointIndex = -1;
  mContextMenuPoint = QPointF();
}

void ReosEditableProfile::deletePoint()
{
  mModel.removePoint( mContextMenuTargetPointIndex );
}

void ReosEditableProfile::insertPoint()
{
  bool found = false;
  int i = 0;
  const QPolygonF &points = mModel.points();

  while ( ( !found ) && i < points.count() )
  {
    found = mContextMenuPoint.x() < points.at( i ).x();
    if ( !found )
      ++i;
  }

  mModel.insertPoint( i, mContextMenuPoint );
}

void ReosEditableProfile::zoomExtent()
{
  if ( !mPlot )
    return;

  const QPolygonF &points = mModel.points();

  if ( points.count() < 2 )
    return;

  QRectF bbox = points.boundingRect();

  mPlot->setAxisScale( QwtPlot::xBottom, bbox.left(), bbox.right() );
  mPlot->setAxisScale( QwtPlot::yLeft, bbox.top(), bbox.bottom() );
  mPlot->replot();
  mPlot->resetZoomBase();
}

void ReosEditableProfile::addPoint( const QPointF &point )
{
  mModel.addPoint( point );
  emit itemChanged();
}

void ReosEditableProfile::setProfile( const QPolygonF &prof )
{
  mModel.setPoints( prof );
  emit itemChanged();
}

ReosEditableProfileModel::ReosEditableProfileModel( QObject *parent ): QAbstractTableModel( parent )
{
}

int ReosEditableProfileModel::rowCount( const QModelIndex &parent ) const
{
  Q_UNUSED( parent );
  return mPoints.count() + 1; //last is kept empty to allow user to add point
}

int ReosEditableProfileModel::columnCount( const QModelIndex &parent ) const
{
  Q_UNUSED( parent );
  return 2;
}

QVariant ReosEditableProfileModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( role == Qt::DisplayRole || role == Qt::EditRole )
  {
    if ( index.row() == mPoints.count() )
    {
      if ( index.column() == 0 )
      {
        return mTemporaryNewX;
      }

      if ( index.column() == 1 )
      {
        return mTemporaryNewY;
      }
    }
    else if ( index.row() < mPoints.count() )
    {
      if ( index.column() == 0 )
      {
        return  ReosParameter::doubleToString( mPoints.at( index.row() ).x(),  0 );
      }

      if ( index.column() == 1 )
      {
        return  ReosParameter::doubleToString( mPoints.at( index.row() ).y(),  2 );
      }
    }
  }

  return QVariant();
}

bool ReosEditableProfileModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() )
    return false;

  if ( value.toString() == "" )
    return false;

  if ( role == Qt::EditRole )
  {
    if ( index.row() == mPoints.count() )
    {
      if ( index.column() == 0 )
      {
        mTemporaryNewX = value.toString();
      }

      if ( index.column() == 1 )
      {
        mTemporaryNewY = value.toString();
      }

      bool okX, okY;
      double x = ReosParameter::stringToDouble( mTemporaryNewX, &okX );
      double y = ReosParameter::stringToDouble( mTemporaryNewY, &okY );
      if ( okX && okY )
      {
        int r = index.row();
        beginInsertRows( QModelIndex(), r, r );
        mPoints.append( {x, y} );
        mTemporaryNewX.clear();
        mTemporaryNewY.clear();
        endInsertRows();
        emit pointInserted( r );
      }

    }
    else if ( index.row() < mPoints.count() )
    {
      if ( index.column() == 0 )
      {
        bool ok = false;
        double x = ReosParameter::stringToDouble( value.toString(), &ok );
        if ( ok )
        {
          mPoints[index.row()].setX( x );
          emit pointChanged( index.column() );
          return true;
        }
      }

      if ( index.column() == 1 )
      {
        bool ok = false;
        double y = ReosParameter::stringToDouble( value.toString(), &ok );
        if ( ok )
        {
          mPoints[index.row()].setY( y );
          emit pointChanged( index.column() );
          return true;
        }
      }
    }
  }

  return true;
}

QVariant ReosEditableProfileModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( role == Qt::DisplayRole )
  {
    if ( orientation == Qt::Horizontal )
    {
      if ( section == 0 )
      {
        return tr( "Distance" );
      }
      if ( section == 1 )
      {
        return tr( "Altitude" );
      }
    }
  }


  return QVariant();
}

Qt::ItemFlags ReosEditableProfileModel::flags( const QModelIndex &index ) const
{
  return Qt::ItemIsEditable | QAbstractTableModel::flags( index );
}

void ReosEditableProfileModel::clear()
{
  beginResetModel();
  mPoints.clear();
  endResetModel();
}

void ReosEditableProfileModel::setPoints( const QPolygonF &points )
{
  beginResetModel();
  mPoints = points;
  endResetModel();
}

int ReosEditableProfileModel::pointCount() const
{
  return mPoints.count();
}

const QPolygonF &ReosEditableProfileModel::points() const
{
  return mPoints;
}

void ReosEditableProfileModel::insertPoint( int i, const QPointF &point )
{
  beginInsertRows( QModelIndex(), i, i );
  mPoints.insert( i, point );
  endInsertRows();
  emit pointInserted( i );
}

void ReosEditableProfileModel::addPoint( const QPointF &point )
{
  beginInsertRows( QModelIndex(), mPoints.count(), mPoints.count() );
  mPoints.append( point );
  endInsertRows();
  emit pointInserted( mPoints.count() - 1 );
}

void ReosEditableProfileModel::removePoint( int i )
{
  beginRemoveRows( QModelIndex(), i, i );
  mPoints.removeAt( i );
  endRemoveRows();
  emit pointRemoved( i );
}

int ReosEditableProfileModel::pointIndexIn( const QRectF &rect ) const
{
  bool found = false;
  int index = 0;
  while ( ( !found ) && ( index < mPoints.count() ) )
  {
    found = rect.contains( mPoints.at( index ) );
    if ( !found )
      ++index;
  }

  if ( !found )
    index = -1;

  return index;
}

void ReosEditableProfileModel::movePoint( int i, const QPointF &newPos )
{
  if ( i >= 0 && i < mPoints.count() )
  {
    mPoints.replace( i, newPos );
    emit dataChanged( createIndex( i, 0 ), createIndex( i, 1 ) );
  }
}
