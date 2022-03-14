/***************************************************************************
  reosplotitemlist.cpp

 ---------------------
 begin                : 19.11.2021
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
#include "reosplotitemlist.h"

#include <qgscolorwidgets.h>

#include <QListView>
#include <QMenu>
#include <QPainter>
#include <QMouseEvent>

#include "reosplotwidget.h"
#include "reostimeserie.h"


ReosPlotItemListModel::ReosPlotItemListModel( ReosPlotWidget *plotWidget, QObject *parent )
  : QAbstractListModel( parent )
  , mPlotWidget( plotWidget )
{}

QModelIndex ReosPlotItemListModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column, std::get<1>( mPlot.at( row ) ) );
}

QModelIndex ReosPlotItemListModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosPlotItemListModel::rowCount( const QModelIndex & ) const
{
  return mPlot.count();
}

int ReosPlotItemListModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosPlotItemListModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() >= mPlot.count() )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return std::get<1>( mPlot.at( index.row() ) )->name();
      break;
    case Qt::CheckStateRole:
      return std::get<2>( mPlot.at( index.row() ) ) ?  Qt::CheckState::Checked : Qt::CheckState::Unchecked;
      break;
    case Qt::DecorationRole:
      return std::get<0>( mPlot.at( index.row() ) )->icone( QSize( 20, 12 ) );
      break;
    default:
      return QVariant();
  }

  return QVariant();

}

bool ReosPlotItemListModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() )
    return false;

  if ( index.row() >= mPlot.count() )
    return false;

  if ( role == Qt::CheckStateRole )
  {
    if ( value == Qt::CheckState::Checked )
      mPlot[index.row()] = std::make_tuple( std::get<0>( mPlot[index.row() ] ), std::get<1>( mPlot[index.row() ] ), true );
    else if ( value == Qt::CheckState::Unchecked )
      mPlot[index.row()] = std::make_tuple( std::get<0>( mPlot[index.row() ] ), std::get<1>( mPlot[index.row() ] ), false );

    updateItemVisibility( index.row(), true );
    return true;
  }

  return false;
}
Qt::ItemFlags ReosPlotItemListModel::flags( const QModelIndex &index ) const
{
  return QAbstractItemModel::flags( index ) | Qt::ItemIsUserCheckable;
}

ReosPlotItem *ReosPlotItemListModel::addData( ReosTimeSerieVariableTimeStep *data )
{
  beginResetModel();
  std::unique_ptr<ReosPlotItem> item( ReosPlotItemFactories::instance()->buildPlotItem( mPlotWidget, data ) );
  mPlot.append( {item.get(), data, true} );
  mPlotWidget->addPlotItem( item.get() );
  updateItemVisibility( mPlot.count() - 1, true );
  endResetModel();
  return item.release();
}

void ReosPlotItemListModel::clear()
{
  for ( int i = 0; i < mPlot.count(); ++i )
  {
    std::get<0>( mPlot.at( i ) )->detach();
    delete std::get<0>( mPlot.at( i ) );
  }
  mPlot.clear();
}

bool ReosPlotItemListModel::globalVisibilty() const
{
  return mGlobalVisibilty;
}

void ReosPlotItemListModel::setGlobalVisibilty( bool globalVisibilty )
{
  mGlobalVisibilty = globalVisibilty;
  for ( int i = 0; i < mPlot.count() - 1; ++i )
  {
    updateItemVisibility( i, false );
  }

  if ( !mPlot.isEmpty() )
    updateItemVisibility( mPlot.count() - 1, true );
}

void ReosPlotItemListModel::updateItemVisibility( int itemIndex, bool replot )
{
  bool  isVisible = mGlobalVisibilty && std::get<2>( mPlot.at( itemIndex ) );
  std::get<0>( mPlot.at( itemIndex ) )->setLegendActive( isVisible );
  std::get<0>( mPlot.at( itemIndex ) )->setVisible( isVisible, replot );
}


ReosVariableTimeStepPlotListButton::ReosVariableTimeStepPlotListButton( const QString &title, ReosPlotWidget *parent )
  : QToolButton( parent )
{
  setPopupMode( QToolButton::MenuButtonPopup );
  setText( title );
  parent->addOptionalPlotItem( this );

  QMenu *menu = new QMenu( this );
  setMenu( menu );
  mView = new ReosVariableTimeStepPlotListView( parent, this );
  QWidgetAction *widgetAction = new QWidgetAction( this );
  widgetAction->setDefaultWidget( mView );
  menu->addAction( widgetAction );
  connect( this, &QToolButton::toggled, mView, &ReosVariableTimeStepPlotListView::setGlobalVisibility );

  setVisible( true );
  setCheckable( true );
  setEnabled( false );
}

ReosPlotItem *ReosVariableTimeStepPlotListButton::addData( ReosTimeSerieVariableTimeStep *data )
{
  setEnabled( true );
  return mView->addData( data );
}

void ReosVariableTimeStepPlotListButton::clear()
{
  mView->clear();
  setEnabled( false );
}

ReosVariableTimeStepPlotListView::ReosVariableTimeStepPlotListView( ReosPlotWidget *plotWidget, QWidget *parent ): QListView( parent )
{
  mModel = new ReosPlotItemListModel( plotWidget, this );
  setModel( mModel );
}

ReosPlotItem *ReosVariableTimeStepPlotListView::addData( ReosTimeSerieVariableTimeStep *data )
{
  return mModel->addData( data );
}

void ReosVariableTimeStepPlotListView::setGlobalVisibility( bool isItemVisible )
{
  mModel->setGlobalVisibilty( isItemVisible );
}

void ReosVariableTimeStepPlotListView::clear()
{
  mModel->clear();
}

QSize ReosVariableTimeStepPlotListView::sizeHint() const
{
  QSize s = QListView::sizeHint();
  if ( model()->rowCount() == 0 )
    return QSize( s.width(), 0 );
  int itemsCount = model()->rowCount();
  return QSize( s.width(), itemsCount * sizeHintForRow( 0 ) );
}

void ReosVariableTimeStepPlotListView::contextMenuEvent( QContextMenuEvent *event )
{
  QModelIndex index = indexAt( event->pos() );
  if ( !index.isValid() )
    return;

  ReosTimeSerieVariableTimeStep *data = static_cast<ReosTimeSerieVariableTimeStep *>( index.internalPointer() );

  QMenu menu;

  QgsColorWheel *colorWheel = new QgsColorWheel( this );
  QgsColorWidgetAction *colorWheelAction = new QgsColorWidgetAction( colorWheel, &menu, this );

  colorWheel->setColor( data->color() );
  colorWheelAction->setDismissOnColorSelection( false );
  menu.addAction( colorWheelAction );

  connect( colorWheelAction, &QgsColorWidgetAction::colorChanged, data, &ReosTimeSerieVariableTimeStep::setColor );

  menu.exec( mapToGlobal( event->pos() ) );
}
