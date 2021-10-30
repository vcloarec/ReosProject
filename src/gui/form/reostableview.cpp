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

ReosTableView::ReosTableView( QWidget *parent ): QTableView( parent )
{
  setHorizontalHeader( new ReosHorizontalHeaderView( this ) );
}

void ReosTableView::keyPressEvent( QKeyEvent *event )
{
  if ( event->matches( QKeySequence::Paste ) )
  {
    QList<double> values = clipboardToValues();
    if ( !values.isEmpty() )
      emit pastDataFromClipboard( currentIndex(), values );
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

void ReosTableView::contextMenuEvent( QContextMenuEvent *event )
{
  QMenu menu;

  menu.addAction( tr( "Delete selected row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
    {
      emit deleteRows( this->selectionModel()->selectedIndexes().first(), count );
    }
  } );

  menu.addAction( tr( "Insert row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
    {
      emit insertRow( this->selectionModel()->selectedIndexes().first(), count );
    }
  } );

  menu.addAction( tr( "Insert row(s) from clipboard" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    QList<double> values = clipboardToValues();
    if ( values.count() > 0 && count > 0 )
    {
      emit insertRow( this->selectionModel()->selectedIndexes().first(), count );
      emit pastDataFromClipboard( this->selectionModel()->selectedIndexes().first(), values );
    }
  } );

  menu.exec( viewport()->mapToGlobal( event->pos() ) );
}

QList<double> ReosTableView::clipboardToValues()
{
  QClipboard *clipBoard = QApplication::clipboard();
  QString clipBoardText = clipBoard->text();
  if ( QLocale::system().decimalPoint() == ',' )
    clipBoardText.replace( ',', '.' );
  const QStringList lines = clipBoardText.split( "\n" );
  QList<double> values;
  bool ok = true;
  for ( const QString &l : lines )
  {
    if ( l == QString() )
      continue;
    values.append( l.toDouble( &ok ) );
    if ( !ok )
      break;
  }

  if ( !ok )
  {
    QMessageBox::warning( this, tr( "Paste value to table" ), tr( "Incompatible data in the cliboard: " ).arg( clipBoardText ) );
    return QList<double>();
  }
  else
    return values;
}
