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
  mEditableColumn = timeSerieModel->editableColumn();
}

void ReosTimeSerieTableView::keyPressEvent( QKeyEvent *event )
{
  if ( !timeSerieModel() )
    return;

  QModelIndex index = currentIndex();

  if ( event->matches( QKeySequence::Paste ) )
  {
    timeSerieModel()->setValues( index, clipBoardToVariantList() );
  }

  if ( event->key() == Qt::Key_Enter )
  {
    if ( index.isValid() && index.row() < model()->rowCount() )
    {
      setCurrentIndex( model()->index( index.row() + 1, index.column() ) );
      edit( currentIndex() );
    }
    else if ( index.isValid() )
    {
      setCurrentIndex( index );
      edit( currentIndex() );
    }
    resizeColumnsToContents();
    horizontalHeader()->resizeSections( QHeaderView::Stretch );
  }

  QTableView::keyPressEvent( event );
}

void ReosTimeSerieTableView::contextMenuEvent( QContextMenuEvent *event )
{
  QMenu menu;

  menu.addAction( tr( "Delete %n selected rows", nullptr, selectionModel()->selectedRows( 0 ).count() ), this, [this]()
  {
    int count = this->selectionModel()->selectedRows( 0 ).count();
    if ( count > 0 )
      timeSerieModel()->deleteRows( this->selectionModel()->selectedIndexes().first(), count );
  } );

  menu.addAction( tr( "Insert %n rows", nullptr, selectionModel()->selectedRows( 0 ).count() ), this, [this]()
  {
    int count = this->selectionModel()->selectedRows( 0 ).count();
    if ( count > 0 )
      timeSerieModel()->insertRows( this->selectionModel()->selectedIndexes().first(), count );
  } );

  const QList<QVariantList> values = clipBoardToVariantList();
  if ( values.count() > 0 )
  {
    menu.addSeparator();
    menu.addAction( tr( "Insert %n rows from clipboard", nullptr, values.count() ), this, [this, values]()
    {
      int count = this->selectionModel()->selectedIndexes().count();

      if ( values.count() > 0 && count > 0 )
      {
        timeSerieModel()->insertValues( this->selectionModel()->selectedIndexes().first(), values );
      }
    } );

    menu.addAction( tr( "Paste %n rows from clipboard", nullptr, values.count() ), this, [this, values]()
    {
      int count = this->selectionModel()->selectedIndexes().count();
      if ( values.count() > 0 && count > 0 )
      {
        timeSerieModel()->setValues( this->selectionModel()->selectedIndexes().first(), values );
      }
    } );
  }

  menu.addSeparator();
  menu.addAction( tr( "Copy selected values" ), this, [this]
  {
    this->copySelectedToClipBoard( false );
  } );

  menu.addAction( tr( "Copy selected values with headers" ), this, [this]
  {
    this->copySelectedToClipBoard( true );
  } );


  menu.exec( viewport()->mapToGlobal( event->pos() ) );
}

QModelIndex ReosTimeSerieTableView::moveCursor( QAbstractItemView::CursorAction cursorAction, Qt::KeyboardModifiers modifiers )
{
  if ( cursorAction == QAbstractItemView::MoveNext &&
       !mEditableColumn.isEmpty() &&
       currentIndex().column() == mEditableColumn.last() )
  {
    resizeColumnsToContents();
    horizontalHeader()->resizeSections( QHeaderView::Stretch );
    return model()->index( currentIndex().row() + 1, mEditableColumn.first() );
  }

  return QTableView::moveCursor( cursorAction, modifiers );
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

void ReosTimeSerieTableView::copySelectedToClipBoard( bool withHeader )
{
  QItemSelectionModel *selectModel = selectionModel();
  if ( !selectModel )
    return;

  const QItemSelection &selection = selectModel->selection();

  QString copyText;
  if ( !selection.isEmpty() )
  {
    QStringList lines;
    const QItemSelectionRange &range = selection.first();
    if ( withHeader )
    {
      QStringList headers;
      // headers
      for ( int w = 0; w < range.width(); ++w )
        headers.append( model()->headerData( range.left() + w, Qt::Horizontal, Qt::DisplayRole ).toString() );
      lines.append( headers.join( QStringLiteral( "\t" ) ) );
    }
    for ( int h = 0; h < range.height(); ++h )
    {
      QStringList lineData;
      for ( int w = 0; w < range.width(); ++w )
        lineData.append( model()->data(
                           model()->index( range.top() + h, range.left() + w, QModelIndex() ), Qt::DisplayRole ).toString() );

      lines.append( lineData.join( QStringLiteral( "\t" ) ) );
    }
    copyText = lines.join( QStringLiteral( "\n" ) );
  }

  QApplication::clipboard()->setText( copyText );
}
