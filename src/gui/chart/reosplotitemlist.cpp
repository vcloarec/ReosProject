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
#include <QColorDialog>

#include "reosplotwidget.h"


ReosPlotItemListModel::ReosPlotItemListModel( QObject *parent )
  : QAbstractListModel( parent )
{}

QModelIndex ReosPlotItemListModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column, mPlotItems.at( row ).first );
}

QModelIndex ReosPlotItemListModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosPlotItemListModel::rowCount( const QModelIndex & ) const
{
  return mPlotItems.count();
}

int ReosPlotItemListModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosPlotItemListModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() >= mPlotItems.count() )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return mPlotItems.at( index.row() ).first->name();
      break;
    case Qt::CheckStateRole:
      return mPlotItems.at( index.row() ).second ?  Qt::CheckState::Checked : Qt::CheckState::Unchecked;
      break;
    case Qt::DecorationRole:
      return mPlotItems.at( index.row() ).first->icone( QSize( 20, 12 ) );
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

  if ( index.row() >= mPlotItems.count() )
    return false;

  if ( role == Qt::CheckStateRole )
  {
    if ( value == Qt::CheckState::Checked )
      mPlotItems[index.row() ].second = true;
    else if ( value == Qt::CheckState::Unchecked )
      mPlotItems[index.row() ].second = false ;

    mPlotItems.at( index.row() ).first->setVisible( mGlobalVisibilty && mPlotItems.at( index.row() ).second );
    return true;
  }

  return false;
}

Qt::ItemFlags ReosPlotItemListModel::flags( const QModelIndex &index ) const
{
  return QAbstractItemModel::flags( index ) | Qt::ItemIsUserCheckable;
}

void ReosPlotItemListModel::addPlotItem( ReosPlotItem *item )
{
  beginResetModel();
  mPlotItems.append( {item, true} );
  item->setVisible( mGlobalVisibilty );
  endResetModel();
}

void ReosPlotItemListModel::clear()
{
  for ( int i = 0; i < mPlotItems.count(); ++i )
  {
    mPlotItems.at( i ).first->detach();
    delete mPlotItems.at( i ).first;
  }
  mPlotItems.clear();
}

bool ReosPlotItemListModel::globalVisibilty() const
{
  return mGlobalVisibilty;
}

void ReosPlotItemListModel::setGlobalVisibilty( bool globalVisibilty )
{
  mGlobalVisibilty = globalVisibilty;
  for ( int i = 0; i < mPlotItems.count() - 1; ++i )
    mPlotItems.at( i ).first->setVisible( mGlobalVisibilty && mPlotItems.at( i ).second, false );

  if ( !mPlotItems.isEmpty() )
    mPlotItems.last().first->setVisible( mGlobalVisibilty && mPlotItems.last().second );
}

ReosOptionalPlotItemWidgetAction::ReosOptionalPlotItemWidgetAction( ReosPlotWidget *parent )
  : QWidgetAction( parent )
  , mPlotItemsModel( new ReosPlotItemListModel( this ) )
  , mPlotWidget( parent )
{
  setCheckable( true );

  QGridLayout *gLayout = new QGridLayout();
  gLayout->setContentsMargins( 3, 2, 3, 2 );

  QWidget *w = new QWidget();
  w->setLayout( gLayout );
  setDefaultWidget( w );

  mPlotItemListView = new ReosPlotItemListView( w );
  mPlotItemListView->setModel( mPlotItemsModel );
  gLayout->addWidget( mPlotItemListView );
}

void ReosOptionalPlotItemWidgetAction::addPlotItem( ReosPlotItem *plotItem )
{
  mPlotWidget->addPlotItem( plotItem );
  mPlotItemsModel->addPlotItem( plotItem );
}

void ReosOptionalPlotItemWidgetAction::clear()
{
  mPlotItemsModel->clear();
}

bool ReosOptionalPlotItemWidgetAction::globalVisibilty() const
{
  return mPlotItemsModel->globalVisibilty();
}

void ReosOptionalPlotItemWidgetAction::setGlobalVisibilty( bool globalVisibilty )
{
  mPlotItemsModel->setGlobalVisibilty( globalVisibilty );
}

ReosOptionalPlotItemButton::ReosOptionalPlotItemButton( const QString &title, ReosPlotWidget *parent )
  : QToolButton( parent )
{
  setPopupMode( QToolButton::MenuButtonPopup );
  setText( title );
  parent->addOptionalPlotItem( this );

  QMenu *menu = new QMenu( this );
  setMenu( menu );
  mWidgetAction = new ReosOptionalPlotItemWidgetAction( parent );
  menu->addAction( mWidgetAction );
  connect( this, &QToolButton::toggled, mWidgetAction, &ReosOptionalPlotItemWidgetAction::setGlobalVisibilty );

  setVisible( true );
  setCheckable( true );
}

void ReosOptionalPlotItemButton::addPlotItem( ReosPlotItem *plotItem )
{
  mWidgetAction->addPlotItem( plotItem );
}

void ReosOptionalPlotItemButton::clear()
{
  mWidgetAction->clear();
}

ReosPlotItemListView::ReosPlotItemListView( QWidget *parent ): QListView( parent )
{
}

QSize ReosPlotItemListView::sizeHint() const
{
  QSize s = QListView::sizeHint();
  if ( model()->rowCount() == 0 )
    return QSize( s.width(), 0 );
  int itemsCount = model()->rowCount();
  return QSize( s.width(), itemsCount * sizeHintForRow( 0 ) );
}

void ReosPlotItemListView::contextMenuEvent( QContextMenuEvent *event )
{
  QModelIndex index = indexAt( event->pos() );
  if ( !index.isValid() )
    return;

  ReosPlotItem *item = static_cast<ReosPlotItem *>( index.internalPointer() );

  QMenu menu;

  QgsColorWheel *colorWheel = new QgsColorWheel( this );
  QgsColorWidgetAction *colorWheelAction = new QgsColorWidgetAction( colorWheel, &menu, this );
  colorWheel->setColor( item->color() );
  colorWheelAction->setDismissOnColorSelection( false );
  menu.addAction( colorWheelAction );

  connect( colorWheelAction, &QgsColorWidgetAction::colorChanged, item, &ReosPlotItem::setColor );

  menu.exec( mapToGlobal( event->pos() ) );
}

ReosColorWidgetAction::ReosColorWidgetAction( QObject *parent ): QWidgetAction( parent )
{
  mColorWidget = new QColorDialog;
  mColorWidget->setOptions( QColorDialog::NoButtons );
  setDefaultWidget( mColorWidget );

  connect( mColorWidget, &QColorDialog::currentColorChanged, this, &ReosColorWidgetAction::colorChanged );
}

void ReosColorWidgetAction::setColor( const QColor &color )
{
  mColorWidget->setCurrentColor( color );
}
