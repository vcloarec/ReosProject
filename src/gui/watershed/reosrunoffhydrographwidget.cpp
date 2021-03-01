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
#include <QLabel>

#include "reostransferfunction.h"
#include "reosrunoffmodel.h"
#include "reosparameter.h"
#include "reoswatershed.h"
#include "reoswatershedmodule.h"
#include "reosmeteorologicmodel.h"
#include "reosplottimeconstantinterval.h"
#include "reosrunoffhydrographwidget.h"

ReosRunoffHydrographWidget::ReosRunoffHydrographWidget( ReosWatershedModule *watershedModule, QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosRunoffHydrographWidget )
  , mWatershedModule( watershedModule )
  , mWatershedRunoffModelsModel( new ReosWatershedRunoffModelsModel( this ) )
  , mRunoffResultTabModel( new ReosRunoffResultModel( this ) )
  , mHydrographResultModel( new ReosReosHydrographResultModel( this ) )
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

  mRainfallHistogram = new ReosPlotTimeHistogram( tr( "Rainfall" ), true );
  mRainfallHistogram->setBorderColor( Qt::blue );
  mRainfallHistogram->setBrushStyle( Qt::NoBrush );
  mRainfallHistogram->setBorderWdidth( 1.5 );
  mRunoffHistogram = new ReosPlotTimeHistogram( tr( "Runoff" ), false );
  mRunoffHistogram->setBrushColor( QColor( 250, 150, 0, 175 ) );
  mHydrographCurve = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  mHydrographCurve->setOnRightAxe();
  ui->widgetPlot->addPlotItem( mRainfallHistogram );
  ui->widgetPlot->addPlotItem( mRunoffHistogram );
  ui->widgetPlot->addPlotItem( mHydrographCurve );
  ui->widgetPlot->setTitleAxeX( tr( "Time" ) );
  ui->widgetPlot->setAxeXType( ReosPlotWidget::temporal );
  ui->widgetPlot->enableAxeYright( true );
  ui->widgetPlot->setTitleAxeYRight( tr( "Flow rate (m\u00B3/s)" ) );

  ui->comboBoxMeteoModel->setModel( mWatershedModule->meteoModelsCollection() );
  connect( ui->comboBoxMeteoModel, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosRunoffHydrographWidget::onModelMeteoChanged );

  if ( ReosTransferFunctionFactories::isInstantiate() )
  {
    ui->comboBoxTransferFunction->setModel( ReosTransferFunctionFactories::instance()->listModel() );
    ui->comboBoxTransferFunction->setCurrentIndex( 0 );
    connect( ui->comboBoxTransferFunction, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosRunoffHydrographWidget::onTransferFunctionChanged );
  }

  if ( ReosFormWidgetFactories::isInstantiate() )
  {
    ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormLinearReservoirWidgetFactory );
    ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormGeneralizedRationalMethodWidgetFactory );
  }

  ui->tableViewRunoffResult->setModel( mRunoffResultTabModel );
  ui->tableViewRunoffResult->horizontalHeader()->setStretchLastSection( true );
  ui->tableViewHydrographResult->setModel( mHydrographResultModel );
  ui->tableViewHydrographResult->horizontalHeader()->setStretchLastSection( true );

}

ReosRunoffHydrographWidget::~ReosRunoffHydrographWidget()
{
  delete ui;
}

void ReosRunoffHydrographWidget::setCurrentWatershed( ReosWatershed *watershed )
{
  mCurrentWatershed = watershed;

  if ( !mCurrentWatershed )
  {
    mWatershedRunoffModelsModel->setWatershedRunoffModels( nullptr );
    mCurrentTransferFunction = nullptr;
    syncTransferFunction( nullptr );
  }
  else
  {
    mWatershedRunoffModelsModel->setWatershedRunoffModels( watershed->runoffModels() );
    ui->tableViewRunoff->horizontalHeader()->setSectionResizeMode( 0, QHeaderView::ResizeToContents );
    syncTransferFunction( watershed->currentTransferFunction() );
  }

  updateRainall();
}

void ReosRunoffHydrographWidget::setCurrentMeteorologicModel( int index )
{
  ui->comboBoxMeteoModel->setCurrentIndex( index );
}

void ReosRunoffHydrographWidget::onModelMeteoChanged()
{
  mCurrentMeteoModel = mWatershedModule->meteoModelsCollection()->meteorologicModel( ui->comboBoxMeteoModel->currentIndex() );
  updateRainall();
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

void ReosRunoffHydrographWidget::updateRainall()
{
  if ( mCurrentWatershed )
  {
    mCurrentRunoff->deleteLater();
    mCurrentRunoff = nullptr;
  }


  ReosRainfallSerieRainfallItem *rainfall = nullptr;

  if ( mCurrentMeteoModel && mCurrentWatershed )
    rainfall = mCurrentMeteoModel->associatedRainfall( mCurrentWatershed );

  if ( rainfall )
  {
    mRainfallHistogram->setTimeSerie( rainfall->data() );
    ui->labelRainfAllInfo->setText( rainfall->rainfallInformation() );
    mRunoffResultTabModel->setRainFall( rainfall->data() );
    ui->tableViewRunoffResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewRunoffResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
  }
  else
  {
    mRainfallHistogram->setTimeSerie( nullptr );
    ui->labelRainfAllInfo->setText( QString() );
    mRunoffResultTabModel->setRainFall( nullptr );
  }

  if ( mCurrentWatershed && rainfall )
    mCurrentRunoff = new ReosRunoff( mCurrentWatershed->runoffModels(), rainfall->data(), this );

  updateRunoff();
}

void ReosRunoffHydrographWidget::updateRunoff()
{
  if ( mCurrentRunoff )
  {
    mCurrentRunoff->updateValues(); /// TODO : doing that under a parallel process with progress bar
    mRunoffHistogram->setTimeSerie( mCurrentRunoff->data() );
    mRunoffResultTabModel->setRunoff( mCurrentRunoff->data() );
    ui->tableViewRunoffResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewRunoffResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
    connect( mCurrentRunoff, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateHydrograph );
  }
  else
  {
    mRunoffHistogram->setTimeSerie( nullptr );
    mRunoffResultTabModel->setRunoff( nullptr );
  }

  updateHydrograph();
}

void ReosRunoffHydrographWidget::updateHydrograph()
{
  if ( mCurrentHydrograph )
  {
    mCurrentHydrograph->deleteLater();
    mCurrentHydrograph = nullptr;
  }

  if ( mCurrentRunoff && mCurrentTransferFunction )
    mCurrentHydrograph = mCurrentTransferFunction->applyFunction( mCurrentRunoff, this );

  mHydrographCurve->setTimeSerie( mCurrentHydrograph );

  mHydrographResultModel->setHydrograph( mCurrentHydrograph );
  ui->tableViewHydrographResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
  ui->tableViewHydrographResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
}

void ReosRunoffHydrographWidget::onTransferFunctionChanged()
{
  if ( !ReosTransferFunctionFactories::isInstantiate() )
    return;

  ReosTransferFunctionFactories *factories = ReosTransferFunctionFactories::instance();
  int currentIndex = ui->comboBoxTransferFunction->currentIndex();

  if ( mCurrentWatershed && currentIndex >= 0 )
  {
    QString type = factories->type( currentIndex );
    mCurrentWatershed->setCurrentTransferFunction( type );
    syncTransferFunction( mCurrentWatershed->currentTransferFunction() );
  }
  else
    syncTransferFunction( nullptr );
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

  if ( row < mWatershedRunoffModelsModel->runoffCount() && mWatershedRunoffModelsModel->canBeRemoved( row ) )
    menu->addAction( tr( "Remove this model" ), menu, [this, row]
  {
    mWatershedRunoffModelsModel->removeRunoffModel( row );
  } );


}

void ReosRunoffHydrographWidget::syncTransferFunction( ReosTransferFunction *function )
{
  if ( ! ReosTransferFunctionFactories::isInstantiate() )
    return;

  ReosTransferFunctionFactories *factories = ReosTransferFunctionFactories::instance();
  int index = -1;
  if ( function )
  {
    index = factories->index( function->type() );
    if ( index < 0 )
    {
      function = nullptr;
    }
  }
  ui->comboBoxTransferFunction->setCurrentIndex( index );

  ReosFormWidget *oldForm = mCurrentTransferFunctionForm;
  if ( oldForm )
  {
    ui->widgetTransferFunction->layout()->removeWidget( oldForm );
    oldForm->deleteLater();
  }

  if ( mCurrentTransferFunction )
    disconnect( mCurrentTransferFunction, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateHydrograph );

  mCurrentTransferFunction = function;

  if ( mCurrentTransferFunction )
    connect( mCurrentTransferFunction, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateHydrograph );

  mCurrentTransferFunctionForm = ReosFormWidgetFactories::instance()->createDataFormWidget( function );
  if ( mCurrentTransferFunctionForm )
    ui->widgetTransferFunction->layout()->addWidget( mCurrentTransferFunctionForm );

  updateHydrograph();
}


void ReosWatershedRunoffModelsModel::setWatershedRunoffModels( ReosRunoffModelsGroup *watershedRunoffModels )
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

bool ReosWatershedRunoffModelsModel::canBeRemoved( int row )
{
  return !mWatershedRunoffModels->isLocked( row ) && portionEditable();
}

void ReosWatershedRunoffModelsModel::removeRunoffModel( int row )
{
  beginRemoveRows( QModelIndex(), row, 0 );
  mWatershedRunoffModels->removeRunoffModel( row );
  endRemoveRows();
  allDataChanged();
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

    double v = mWatershedRunoffModels->coefficient( i )->value();

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
      double old = mWatershedRunoffModels->coefficient( i )->value();
      if ( old >= dif )
      {
        mWatershedRunoffModels->coefficient( i )->setValue( old - dif );
        break;
      }
      else
      {
        mWatershedRunoffModels->coefficient( i )->setValue( 0.0 );
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
      ReosParameterDouble *portion = mWatershedRunoffModels->coefficient( i );
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
      ReosParameterDouble *portion = mWatershedRunoffModels->coefficient( i );
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

ReosFormWidget *ReosFormLinearReservoirWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTransferFunctionLinearReservoir *transferFunction = qobject_cast<ReosTransferFunctionLinearReservoir *>( dataObject );
  if ( !transferFunction )
    return nullptr;

  std::unique_ptr<ReosFormWidget> form = std::make_unique<ReosFormWidget>( parent );

  form->addParameter( transferFunction->area(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->concentrationTime(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->useConcentrationTime(), -1, ReosParameterWidget::SpacerAfter );
  ReosParameterWidget *factorWidget = form->addParameter( transferFunction->factorToLagTime(), -1, ReosParameterWidget::SpacerInMiddle );
  ReosParameterWidget *lagTimeWidget = form->addParameter( transferFunction->lagTime(), -1, ReosParameterWidget::SpacerInMiddle );
  QLabel *lagTimeDeduced = new QLabel( form.get() );
  form->addWidget( lagTimeDeduced );

  bool useConcTime = transferFunction->useConcentrationTime()->value();
  factorWidget->setVisible( useConcTime );
  lagTimeWidget->setVisible( !useConcTime );

  QString lagTimeDeducedtext = QObject::tr( "Lag time from concentration time: %1" );
  ReosDuration concTime = transferFunction->concentrationTime()->value();
  double fact = transferFunction->factorToLagTime()->value();
  lagTimeDeduced->setText( lagTimeDeducedtext.arg( ( concTime * fact ).toString( 2 ) ) );

  QObject::connect( transferFunction->useConcentrationTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, factorWidget, lagTimeWidget, lagTimeDeduced]
  {
    bool useConcTime = transferFunction->useConcentrationTime()->value();
    factorWidget->setVisible( useConcTime );
    lagTimeDeduced->setVisible( useConcTime );
    lagTimeWidget->setVisible( !useConcTime );
  } );

  QObject::connect( transferFunction->concentrationTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, lagTimeDeduced, lagTimeDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    double fact = transferFunction->factorToLagTime()->value();
    lagTimeDeduced->setText( lagTimeDeducedtext.arg( ( concTime * fact ).toString( 2 ) ) );
  } );

  QObject::connect( transferFunction->concentrationTime(), &ReosParameter::unitChanged, form.get(), [transferFunction, lagTimeDeduced, lagTimeDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    double fact = transferFunction->factorToLagTime()->value();
    lagTimeDeduced->setText( lagTimeDeducedtext.arg( ( concTime * fact ).toString( 2 ) ) );
  } );

  QObject::connect( transferFunction->factorToLagTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, lagTimeDeduced, lagTimeDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    double fact = transferFunction->factorToLagTime()->value();
    lagTimeDeduced->setText( lagTimeDeducedtext.arg( ( concTime * fact ).toString( 2 ) ) );
  } );

  return form.release();
}

ReosFormWidget *ReosFormGeneralizedRationalMethodWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTransferFunctionGeneralizedRationalMethod *transferFunction = qobject_cast<ReosTransferFunctionGeneralizedRationalMethod *>( dataObject );
  if ( !transferFunction )
    return nullptr;

  std::unique_ptr<ReosFormWidget> form = std::make_unique<ReosFormWidget>( parent );

  form->addParameter( transferFunction->area(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->concentrationTime(), -1, ReosParameterWidget::SpacerInMiddle );

  return form.release();
}

ReosRunoffResultModel::ReosRunoffResultModel( QObject *parent ): QAbstractTableModel( parent ) {}

QModelIndex ReosRunoffResultModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosRunoffResultModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosRunoffResultModel::rowCount( const QModelIndex & ) const
{
  if ( !mRainfall.isNull() )
    return mRainfall->valueCount();

  return 0;
}

int ReosRunoffResultModel::columnCount( const QModelIndex & ) const
{
  if ( !mRunoff.isNull() )
    return 3;
  if ( !mRainfall.isNull() )
    return 2;

  return 0;

}

QVariant ReosRunoffResultModel::data( const QModelIndex &index, int role ) const
{
  if ( index.row() >= rowCount( QModelIndex() ) )
    return QVariant();

  int row = index.row();

  if ( role == Qt::DisplayRole )
  {
    switch ( index.column() )
    {
      case 0: //time
        if ( !mRainfall.isNull() && row < mRainfall->valueCount() )
          return mRainfall->timeAt( row ).toString( "yyyy.MM.dd hh:mm:ss" );
        break;
      case 1: //rainfall
        if ( !mRainfall.isNull() && row < mRainfall->valueCount() )
          return QString::number( mRainfall->valueAt( row ), 'f', 2 );
        break;
      case 2: //runoff
        if ( !mRunoff.isNull() && row < mRunoff->valueCount() )
          return QString::number( mRunoff->valueAt( row ), 'f', 2 );
        break;
      default:
        return QVariant();
        break;;
    }
  }

  if ( role == Qt::TextAlignmentRole )
  {
    return Qt::AlignHCenter;
  }

  return QVariant();
}

QVariant ReosRunoffResultModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    switch ( section )
    {
      case 0: //time
        return tr( "Time" );
        break;
      case 1: //rainfall
        return tr( "Rainfall %1" ).arg( mRainfall->unitStringCurrentMode() );
        break;
      case 2: //runoff
        return tr( "Runoff %1" ).arg( mRunoff->unitStringCurrentMode() );
        break;
        break;
      default:
        return QVariant();
        break;;
    }
  }

  return QVariant();
}

void ReosRunoffResultModel::setRainFall( ReosSerieRainfall *rainfall )
{
  beginResetModel();
  mRainfall = rainfall;
  mRunoff = nullptr;
  endResetModel();

}

void ReosRunoffResultModel::setRunoff( ReosTimeSerieConstantInterval *runoff )
{
  beginResetModel();
  mRunoff = runoff;
  endResetModel();
}


ReosReosHydrographResultModel::ReosReosHydrographResultModel( QObject *parent ): QAbstractTableModel( parent )
{

}

QModelIndex ReosReosHydrographResultModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosReosHydrographResultModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosReosHydrographResultModel::rowCount( const QModelIndex & ) const
{
  if ( mHydrograph.isNull() )
    return 0;
  else
    return mHydrograph->valueCount();
}

int ReosReosHydrographResultModel::columnCount( const QModelIndex & ) const
{
  if ( mHydrograph.isNull() )
    return 0;
  else
    return 2;
}

QVariant ReosReosHydrographResultModel::data( const QModelIndex &index, int role ) const
{
  if ( index.row() >= rowCount( QModelIndex() ) )
    return QVariant();

  int row = index.row();

  if ( role == Qt::DisplayRole )
  {
    switch ( index.column() )
    {
      case 0: //time
        if ( !mHydrograph.isNull() && row < mHydrograph->valueCount() )
          return mHydrograph->timeAt( row ).toString( "yyyy.MM.dd hh:mm:ss" );
        break;
      case 1: //flow rate
        if ( !mHydrograph.isNull() && row < mHydrograph->valueCount() )
          return QString::number( mHydrograph->valueAt( row ), 'f', 2 );
        break;
      default:
        return QVariant();
        break;;
    }
  }

  if ( role == Qt::TextAlignmentRole )
  {
    return Qt::AlignHCenter;
  }

  return QVariant();
}

QVariant ReosReosHydrographResultModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    switch ( section )
    {
      case 0: //time
        return tr( "Time" );
        break;
      case 1: //flow rate
        return tr( "Flow rate (m\u00B3/s)" );
        break;
      default:
        return QVariant();
        break;;
    }
  }

  return QVariant();
}

void ReosReosHydrographResultModel::setHydrograph( ReosHydrograph *hydrograph )
{
  beginResetModel();
  mHydrograph = hydrograph;
  endResetModel();
}