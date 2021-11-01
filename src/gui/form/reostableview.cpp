/***************************************************************************
  reostableview.cpp - ReosTableView

 ---------------------
 begin                : 26.10.2021
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
#include "reostableview.h"

#include <QApplication>
#include <QMenu>
#include <QKeyEvent>
#include <QMessageBox>
#include <QClipboard>

#include "reostimeserie.h"

ReosHorizontalHeaderView::ReosHorizontalHeaderView( QWidget *parent ) : QHeaderView( Qt::Horizontal, parent )
{
  setSectionResizeMode( QHeaderView::ResizeToContents );
  setStretchLastSection( true );
  setCascadingSectionResizes( true );
  setDefaultAlignment( Qt::AlignCenter | ( Qt::Alignment )Qt::TextWordWrap );
}

QSize ReosHorizontalHeaderView::sectionSizeFromContents( int logicalIndex ) const
{
  //https://stackoverflow.com/questions/45084542/qtableview-header-word-wrap
  const QString text = this->model()->headerData( logicalIndex, this->orientation(), Qt::DisplayRole ).toString();
  const int maxWidth = this->sectionSize( logicalIndex );
  const int maxHeight = 5000; // arbitrarily large
  const auto alignment = defaultAlignment();
  const QFontMetrics metrics( this->fontMetrics() );
  const QRect rect = metrics.boundingRect( QRect( 0, 0, maxWidth, maxHeight ), alignment, text );

  const QSize textMarginBuffer( 2, 2 ); // buffer space around text preventing clipping
  return rect.size() + textMarginBuffer;
}

ReosTimeSerieTableView::ReosTimeSerieTableView( QWidget *parent ): QTableView( parent )
{
  setHorizontalHeader( new ReosHorizontalHeaderView( this ) );
  setSelectionBehavior( QAbstractItemView::SelectRows );
  setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
}

void ReosTimeSerieTableView::setModel( QAbstractItemModel *model )
{
  ReosTimeSerieModel *timeSerieModel = qobject_cast<ReosTimeSerieModel *>( model );
  if ( !timeSerieModel )
    return;

  QTableView::setModel( model );
}

void ReosTimeSerieTableView::keyPressEvent( QKeyEvent *event )
{
  if ( !timeSerieModel() )
    return;

  if ( event->matches( QKeySequence::Paste ) )
  {
    timeSerieModel()->setValues( currentIndex(), clipBoardToVariantList() );
  }

  QTableView::keyPressEvent( event );

  if ( event->key() == Qt::Key_Enter )
  {
    QModelIndex index = currentIndex();
    if ( index.isValid() && index.row() < model()->rowCount() )
      setCurrentIndex( model()->index( index.row() + 1, index.column() ) );

    resizeColumnsToContents();
    horizontalHeader()->resizeSections( QHeaderView::Stretch );
  }
}

void ReosTimeSerieTableView::contextMenuEvent( QContextMenuEvent *event )
{
  QMenu menu;

  menu.addAction( tr( "Delete selected row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
      timeSerieModel()->deleteRows( this->selectionModel()->selectedIndexes().first(), count );
  } );

  menu.addAction( tr( "Insert row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
      timeSerieModel()->insertRows( this->selectionModel()->selectedIndexes().first(), count );
  } );

  menu.addAction( tr( "Insert row(s) from clipboard" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    const QList<QVariantList> values = clipBoardToVariantList();
    if ( values.count() > 0 && count > 0 )
    {
      timeSerieModel()->insertValues( this->selectionModel()->selectedIndexes().first(), values );
    }
  } );

  menu.exec( viewport()->mapToGlobal( event->pos() ) );
}

QList<QVariantList> ReosTimeSerieTableView::clipBoardToVariantList()
{
  QClipboard *clipBoard = QApplication::clipboard();
  const QString &clipBoardText = clipBoard->text();

  const QStringList &lines = clipBoardText.split( "\n" );

  QList<QVariantList> ret;
  for ( const QString &l : lines )
  {
    if ( l == QString() )
      continue;
    QVariantList row;
    const QStringList &splittedRow = l.split( "\t" );
    for ( const QString &str : splittedRow )
      row.append( str );

    ret.append( row );
  }

  return ret;
}

ReosTimeSerieModel *ReosTimeSerieTableView::timeSerieModel() const
{
  return qobject_cast<ReosTimeSerieModel *>( model() );
}
