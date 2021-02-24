/***************************************************************************
  reosrunoffhydrographwidget.cpp - ReosRunoffHydrographWidget

 ---------------------
 begin                : 22.2.2021
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
#include "reosrunoffhydrographwidget.h"
#include "ui_reosrunoffhydrographwidget.h"

#include <QMenu>

#include "reosrunoffmodel.h"
#include "reosparameter.h"
#include "reoswatershed.h"

ReosRunoffHydrographWidget::ReosRunoffHydrographWidget( QWidget *parent ) :
  ReosActionWidget( parent ),
  ui( new Ui::ReosRunoffHydrographWidget )
  , mWatershedRunoffModelsModel( new ReosWatershedRunoffModelsModel( this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ui->tableViewRunoff->setModel( mWatershedRunoffModelsModel );
  ui->tableViewRunoff->horizontalHeader()->setSectionResizeMode( 0, QHeaderView::Interactive );
  ui->tableViewRunoff->horizontalHeader()->setSectionResizeMode( 1, QHeaderView::ResizeToContents );
  ui->tableViewRunoff->horizontalHeader()->setSectionResizeMode( 2, QHeaderView::ResizeToContents );
  ui->tableViewRunoff->horizontalHeader()->setStretchLastSection( true );

  ui->tableViewRunoff->setContextMenuPolicy( Qt::CustomContextMenu );
  connect( ui->tableViewRunoff, &QWidget::customContextMenuRequested, this,  &ReosRunoffHydrographWidget::onRunoffTableViewContextMenu );
}

ReosRunoffHydrographWidget::~ReosRunoffHydrographWidget()
{
  delete ui;
}

void ReosRunoffHydrographWidget::setCurrentWatershed( ReosWatershed *watershed )
{
  if ( !watershed )
    mWatershedRunoffModelsModel->setWatershedRunoffModels( nullptr );
  else
    mWatershedRunoffModelsModel->setWatershedRunoffModels( watershed->runoffModels() );
}

void ReosRunoffHydrographWidget::onRunoffTableViewContextMenu( const QPoint &pos )
{
  QModelIndex index = ui->tableViewRunoff->indexAt( pos );
  if ( !index.isValid() )
    return;

  if ( index.column() > 0 )
    return;

  int row = index.row();

  QMenu contextMenu;

  buildRunoffChoiceMenu( &contextMenu, row );
  contextMenu.exec( ui->tableViewRunoff->verticalHeader()->mapToGlobal( pos ) );
}

void ReosRunoffHydrographWidget::buildRunoffChoiceMenu( QMenu *menu, int row )
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return;
  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();

  const QStringList types = registery->runoffTypes();

  for ( const QString &type : types )
  {
    ReosRunoffModelCollection collection = registery->runoffModelCollection( type );
    QMenu *typeMenu = new QMenu( collection.displayedText() );
    typeMenu->setIcon( collection.icon() );

    for ( int i = 0; i < collection.runoffModelsCount(); ++i )
    {
      ReosRunoffModel *rom = collection.runoffModel( i );
      typeMenu->addAction( rom->name()->value(), this, [this, rom, row]
      {
        if ( mWatershedRunoffModelsModel->runoffCount() == row )
          mWatershedRunoffModelsModel->addRunoffModel( rom );
        else
          mWatershedRunoffModelsModel->replaceRunoffModel( row, rom );
      } );
    }

    menu->addMenu( typeMenu );
  }

}


void ReosWatershedRunoffModelsModel::setWatershedRunoffModels( ReosWatershedRunoffModels *watershedRunoffModels )
{
  beginResetModel();
  mWatershedRunoffModels = watershedRunoffModels;
  endResetModel();
}

void ReosWatershedRunoffModelsModel::addRunoffModel( ReosRunoffModel *runoffModel )
{
  int insertionPos = mWatershedRunoffModels->runoffModelCount();
  beginInsertRows( QModelIndex(), insertionPos, insertionPos );
  mWatershedRunoffModels->addRunoffModel( runoffModel );
  endInsertRows();
}

void ReosWatershedRunoffModelsModel::replaceRunoffModel( int row, ReosRunoffModel *runoffModel )
{
  mWatershedRunoffModels->replaceRunnofModel( row, runoffModel );
  QModelIndex i = index( row, 0, QModelIndex() );
  emit dataChanged( i, i );
}

int ReosWatershedRunoffModelsModel::runoffCount() const
{
  if ( !mWatershedRunoffModels )
    return 0;
  else
    return mWatershedRunoffModels->runoffModelCount();
}

bool ReosWatershedRunoffModelsModel::portionEditable() const
{
  if ( !mWatershedRunoffModels )
    return false;
  int nonLocked = 0;
  for ( int i = 0; i < mWatershedRunoffModels->runoffModelCount(); ++i )
  {
    if ( !mWatershedRunoffModels->isLocked( i ) )
      nonLocked++;

    if ( nonLocked > 1 )
      return true;
  }

  return false;

}

void ReosWatershedRunoffModelsModel::allDataChanged()
{
  if ( mWatershedRunoffModels )
    emit dataChanged( index( 0, 0, QModelIndex() ), index( mWatershedRunoffModels->runoffModelCount(), 3, QModelIndex() ) );
}

bool ReosWatershedRunoffModelsModel::replacePortion( int position, double portion )
{
  if ( !mWatershedRunoffModels )
    return false;

  if ( portion < 0 )
    return false;

  double total = portion;
  double totalLock = 0;
  for ( int i = 0; i < mWatershedRunoffModels->runoffModelCount(); ++i )
  {
    if ( i == position )
      continue;

    double v = mWatershedRunoffModels->portion( i )->value();

    if ( mWatershedRunoffModels->isLocked( i ) )
      totalLock += v;

    total += v;

    if ( totalLock + portion > 1.0 )
      return false;
  }

  double dif = total - 1.0;

  for ( int i = 0; i < mWatershedRunoffModels->runoffModelCount(); ++i )
  {
    if ( i == position )
      continue;

    if ( !mWatershedRunoffModels->isLocked( i ) )
    {
      double old = mWatershedRunoffModels->portion( i )->value();
      if ( old >= dif )
      {
        mWatershedRunoffModels->portion( i )->setValue( old - dif );
        break;
      }
      else
      {
        mWatershedRunoffModels->portion( i )->setValue( 0.0 );
        dif = dif - old;
      }
    }

  }

  return true;
}

QModelIndex ReosWatershedRunoffModelsModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosWatershedRunoffModelsModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosWatershedRunoffModelsModel::rowCount( const QModelIndex & ) const
{
  if ( !mWatershedRunoffModels )
    return 0;

  return mWatershedRunoffModels->runoffModelCount() + 1;
}

int ReosWatershedRunoffModelsModel::columnCount( const QModelIndex & ) const
{
  return 3;
}

QVariant ReosWatershedRunoffModelsModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( !mWatershedRunoffModels )
    return QVariant();

  if ( index.row() >= mWatershedRunoffModels->runoffModelCount() )
  {
    if ( index.column() == 0 )
    {
      if ( role == Qt::DisplayRole )
        return tr( "Right click to add" );
      if ( role == Qt::ForegroundRole )
        return QColor( Qt::lightGray );
    }
    return QVariant();
  }


  int i = index.row();

  switch ( index.column() )
  {
    case 0:
    {
      ReosRunoffModel *ro = mWatershedRunoffModels->runoffModel( i );
      if ( !ro )
      {
        if ( role == Qt::DisplayRole )
          return tr( "Invalid" );
        if ( role == Qt::BackgroundRole )
          return QColor( 250, 200, 200 );
      }
      else
      {
        if ( role == Qt::DisplayRole )
          return ro->name()->value();
      }
    }
    break;
    case 1:
    {
      ReosParameterDouble *portion = mWatershedRunoffModels->portion( i );
      if ( !portion )
        return QVariant();

      switch ( role )
      {
        case Qt::DisplayRole:
        case  Qt::EditRole:
          return portion->toString( 2 );
          break;
        case Qt::ForegroundRole:
          if ( !portionEditable() )
            return QColor( Qt::darkGray );

          if ( mWatershedRunoffModels->isLocked( i ) )
            return QColor( Qt::darkGray );
          break;
        case Qt::TextAlignmentRole:
          return Qt::AlignCenter;
          break;
        default:
          break;
      }
    }
    break;
    case 2:
      switch ( role )
      {
        case Qt::CheckStateRole:
          return mWatershedRunoffModels->isLocked( i ) ? Qt::Checked : Qt::Unchecked;
          break;
        case Qt::TextAlignmentRole:
          return Qt::AlignHCenter;
        default:
          break;
      }
      break;
    default:
      break;
  }

  return QVariant();
}

bool ReosWatershedRunoffModelsModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !index.isValid() )
    return false;

  if ( !mWatershedRunoffModels )
    return false;

  if ( index.column() == 0 )
    return false;

  if ( index.row() >= mWatershedRunoffModels->runoffModelCount() )
    return false;

  int i = index.row();

  switch ( index.column() )
  {
    case 1:
    {
      ReosParameterDouble *portion = mWatershedRunoffModels->portion( i );
      if ( !portion )
        return false;
      if ( role == Qt::EditRole )
      {
        bool ok = false;
        double v = value.toDouble( &ok );
        if ( ok && replacePortion( i, v ) )
        {
          portion->setValue( value.toDouble() );
          allDataChanged();
          return true;
        }
      }
    }
    break;
    case 2:
      if ( role == Qt::CheckStateRole )
      {
        mWatershedRunoffModels->lock( i, value == Qt::Checked );
        allDataChanged();
        return true;
      }
      break;
    default:
      break;
  }

  return false;

}

Qt::ItemFlags ReosWatershedRunoffModelsModel::flags( const QModelIndex &index ) const
{
  switch ( index.column() )
  {
    case 0:
      return QAbstractTableModel::flags( index );
      break;
    case 1:
      if ( mWatershedRunoffModels && !mWatershedRunoffModels->isLocked( index.row() ) && portionEditable() )
        return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
      break;
    case 2:
      return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable | Qt::ItemIsUserCheckable;
      break;
    default:
      break;
  }

  return QAbstractTableModel::flags( index );
}

QVariant ReosWatershedRunoffModelsModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  switch ( orientation )
  {
    case Qt::Horizontal:
    {
      switch ( role )
      {
        case Qt::DisplayRole:
        {
          switch ( section )
          {
            case 0:
              return tr( "Runoff model" );
              break;
            case 1:
              return tr( "Portion in watershed" );
              break;
            case 2:
              return QVariant();
              break;
            default:
              break;
          }
        }
        break;
        case Qt::DecorationRole:
          if ( section == 2 )
            return QPixmap( QStringLiteral( ":/images/lock.svg" ) );
          break;
        case Qt::TextAlignmentRole:
          return Qt::AlignHCenter;
          break;
      }
    }
    break;

    case Qt::Vertical:
    {
      if ( section < mWatershedRunoffModels->runoffModelCount() )
        return QAbstractTableModel::headerData( section, orientation, role );

      if ( section == mWatershedRunoffModels->runoffModelCount() && role == Qt::DecorationRole )
        return QPixmap( QStringLiteral( ":/images/add.svg" ) );

      return QVariant();
    }
    break;
  }

  return QAbstractTableModel::headerData( section, orientation, role );
}
