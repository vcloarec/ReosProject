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
#include "reosprocesscontroler.h"

ReosRunoffHydrographWidget::ReosRunoffHydrographWidget( ReosWatershedModule *watershedModule, QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosRunoffHydrographWidget )
  , mWatershedModule( watershedModule )
  , mWatershedRunoffModelsModel( new ReosWatershedRunoffModelsModel( this ) )
  , mRunoffResultTabModel( new ReosTimeSeriesTableModel( this ) )
  , mHydrographResultModel( new ReosTimeSeriesVariableTimeStepTabModel( this ) )
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
  ui->widgetPlot->setTitleAxeYRight( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->widgetPlot->setMagnifierType( ReosPlotWidget::positiveMagnifier );

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
    ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormSCSUnithydrographWidgetFactory );
    ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormNashUnithydrographWidgetFactory );
  }

  ui->tableViewRunoffResult->setModel( mRunoffResultTabModel );
  ui->tableViewRunoffResult->horizontalHeader()->setStretchLastSection( true );
  ui->tableViewRunoffResult->setContextMenuPolicy( Qt::CustomContextMenu );
  ui->tableViewHydrographResult->setModel( mHydrographResultModel );
  ui->tableViewHydrographResult->horizontalHeader()->setStretchLastSection( true );
  ui->tableViewHydrographResult->setContextMenuPolicy( Qt::CustomContextMenu );

  connect( ui->tableViewHydrographResult, &QWidget::customContextMenuRequested, this, &ReosRunoffHydrographWidget::hydrographTabContextMenu );
  connect( ui->tableViewRunoffResult, &QWidget::customContextMenuRequested, this, &ReosRunoffHydrographWidget::rainfallRunoffTabContextMenu );

  connect( this, &ReosActionWidget::opened, this, &ReosRunoffHydrographWidget::updateRainall );

  connect( ui->pushButtonTransferFunctionFormulation, &QPushButton::clicked, this, &ReosRunoffHydrographWidget::onTransferFunctionFormulation );

  ui->constantHydrographTimeStep->setDuration( new ReosParameterDuration( QString(), false, this ) );

  connect( ui->checkBoxUseConstantTimeStep, &QCheckBox::toggled, this, [this]
  {
    bool usedConstantTimeStep = ui->checkBoxUseConstantTimeStep->isChecked();

    if ( usedConstantTimeStep && ui->constantHydrographTimeStep->durationParameter()->value() == ReosDuration() && mCurrentRunoff )
      ui->constantHydrographTimeStep->durationParameter()->setValue( mCurrentRunoff->timeStep() );

    if ( mCurrentWatershed )
      mCurrentWatershed->setUsedConstantTimeStepForOutputHydrograph( usedConstantTimeStep );

    ui->constantHydrographTimeStep->setVisible( usedConstantTimeStep );
    mHydrographResultModel->setIsFixedTimeStep( usedConstantTimeStep );

    ui->tableViewHydrographResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewHydrographResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
  } );

  connect( ui->constantHydrographTimeStep, &ReosParameterDurationWidget::valueChanged, this, [this]
  {
    if ( mCurrentWatershed )
      mCurrentWatershed->setTimeStepForOutputHydrograph( ui->constantHydrographTimeStep->durationParameter()->value() );
    mHydrographResultModel->setTimeStep( ui->constantHydrographTimeStep->durationParameter()->value() );

    ui->tableViewHydrographResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewHydrographResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
  } );

  ui->constantHydrographTimeStep->setVisible( ui->checkBoxUseConstantTimeStep->isChecked() );

  onModelMeteoChanged();

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
    ui->checkBoxUseConstantTimeStep->setChecked( watershed->usedConstantTimeStepForOutputHydrograph() );
    ui->constantHydrographTimeStep->durationParameter()->setValue( watershed->timeStepForOutputHydrograph() );
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
  if ( mCurrentMeteoModel )
    disconnect( mCurrentMeteoModel, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateRainall );

  mCurrentMeteoModel = mWatershedModule->meteoModelsCollection()->meteorologicModel( ui->comboBoxMeteoModel->currentIndex() );

  if ( mCurrentMeteoModel )
    connect( mCurrentMeteoModel, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateRainall );

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

static void copyResultValues( ReosTimeSeriesTableModel *model, QItemSelectionModel *selectionModel, bool withHeader )
{
  if ( !selectionModel )
    return;

  const QItemSelection &selection = selectionModel->selection();

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
        headers.append( model->headerData( range.left() + w, Qt::Horizontal, Qt::DisplayRole ).toString() );
      lines.append( headers.join( QStringLiteral( "\t" ) ) );
    }
    for ( int h = 0; h < range.height(); ++h )
    {
      QStringList lineData;
      for ( int w = 0; w < range.width(); ++w )
        lineData.append( model->data(
                           model->index( range.top() + h, range.left() + w, QModelIndex() ), Qt::DisplayRole ).toString() );

      lines.append( lineData.join( QStringLiteral( "\t" ) ) );
    }
    copyText = lines.join( QStringLiteral( "\n" ) );
  }

  QApplication::clipboard()->setText( copyText );
}

static void copyResultHydrographValues( ReosTimeSeriesVariableTimeStepTabModel *model, QItemSelectionModel *selectionModel, bool withHeader )
{
  if ( !selectionModel )
    return;

  const QItemSelection &selection = selectionModel->selection();

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
        headers.append( model->headerData( range.left() + w, Qt::Horizontal, Qt::DisplayRole ).toString() );
      lines.append( headers.join( QStringLiteral( "\t" ) ) );
    }
    for ( int h = 0; h < range.height(); ++h )
    {
      QStringList lineData;
      for ( int w = 0; w < range.width(); ++w )
        lineData.append( model->data(
                           model->index( range.top() + h, range.left() + w, QModelIndex() ), Qt::DisplayRole ).toString() );

      lines.append( lineData.join( QStringLiteral( "\t" ) ) );
    }
    copyText = lines.join( QStringLiteral( "\n" ) );
  }

  QApplication::clipboard()->setText( copyText );
}

void ReosRunoffHydrographWidget::copyHydrographSelected( bool withHeader )
{
  QItemSelectionModel *selectionModel = ui->tableViewHydrographResult->selectionModel();
  copyResultHydrographValues( mHydrographResultModel, selectionModel, withHeader );
}

void ReosRunoffHydrographWidget::copyRainfallRunoffSelected( bool withHeader )
{
  QItemSelectionModel *selectionModel = ui->tableViewRunoffResult->selectionModel();
  copyResultValues( mRunoffResultTabModel, selectionModel, withHeader );
}

void ReosRunoffHydrographWidget::updateRainall()
{
  if ( !isVisible() )
    return;

  if ( mCurrentWatershed )
  {
    mCurrentRunoff->deleteLater();
    mCurrentRunoff = nullptr;
  }

  ReosRainfallSerieRainfallItem *rainfall = nullptr;

  if ( mCurrentMeteoModel && mCurrentWatershed )
    rainfall = mCurrentMeteoModel->associatedRainfall( mCurrentWatershed );

  mRunoffResultTabModel->clearSerie();

  if ( rainfall && rainfall->data() )
  {
    mRainfallHistogram->setTimeSerie( rainfall->data() );
    ui->labelRainfAllInfo->setText( rainfall->rainfallInformation() );
    mRunoffResultTabModel->addTimeSerie( rainfall->data(), tr( "Rainfall %1" ).arg( rainfall->data()->unitStringCurrentMode() ) );
    ui->tableViewRunoffResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewRunoffResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
  }
  else
  {
    mRainfallHistogram->setTimeSerie( nullptr );
    ui->labelRainfAllInfo->setText( QString() );
  }

  if ( mCurrentWatershed && rainfall )
    mCurrentRunoff = new ReosRunoff( mCurrentWatershed->runoffModels(), rainfall->data(), this );

  updateRunoff();
}

void ReosRunoffHydrographWidget::updateRunoff()
{
  if ( !isVisible() )
    return;

  if ( mCurrentRunoff )
  {
    mCurrentRunoff->updateValues(); /// TODO : doing that under a parallel process with progress bar
    mRunoffHistogram->setTimeSerie( mCurrentRunoff->data() );
    mRunoffResultTabModel->addTimeSerie( mCurrentRunoff->data(), tr( "Runoff %1" ).arg( mCurrentRunoff->data()->unitStringCurrentMode() ) );
    ui->tableViewRunoffResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
    ui->tableViewRunoffResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );
    connect( mCurrentRunoff, &ReosDataObject::dataChanged, this, &ReosRunoffHydrographWidget::updateHydrograph );
  }
  else
  {
    mRunoffHistogram->setTimeSerie( nullptr );
  }

  updateHydrograph();
}

void ReosRunoffHydrographWidget::updateHydrograph()
{
  if ( !isVisible() )
    return;

  if ( mCurrentHydrograph )
  {
    mCurrentHydrograph->deleteLater();
    mCurrentHydrograph = nullptr;
  }

  if ( mCurrentRunoff && mCurrentTransferFunction )
  {
    std::unique_ptr<ReosTransferFunctionCalculation> calculation;
    calculation.reset( mCurrentTransferFunction->calculationProcess( mCurrentRunoff ) );
    if ( calculation )
    {
      QApplication::setOverrideCursor( Qt::WaitCursor );
      calculation->start();
      mCurrentHydrograph = calculation->hydrograph();
      mCurrentHydrograph->setColor( Qt::red );
      QApplication::restoreOverrideCursor();
    }
  }

  mHydrographCurve->setTimeSerie( mCurrentHydrograph );

  mHydrographResultModel->clearSerie();
  if ( mCurrentHydrograph )
    mHydrographResultModel->addTimeSerie( mCurrentHydrograph, tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );

  ui->tableViewHydrographResult->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
  ui->tableViewHydrographResult->verticalHeader()->resizeSections( QHeaderView::ResizeToContents );

  if ( mCurrentWatershed &&
       ui->checkBoxUseConstantTimeStep->isChecked() &&
       mCurrentWatershed->timeStepForOutputHydrograph() == ReosDuration() )
  {
    ui->constantHydrographTimeStep->durationParameter()->setValue( mCurrentRunoff->timeStep() );
    mCurrentWatershed->setTimeStepForOutputHydrograph( mCurrentRunoff->timeStep() );
  }

  ui->widgetPlot->updatePlot();
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

void ReosRunoffHydrographWidget::hydrographTabContextMenu( const QPoint &pos )
{
  QMenu contextMenu;

  contextMenu.addAction( tr( "Copy selected values" ), &contextMenu, [this]
  {
    this->copyHydrographSelected( false );
  } );

  contextMenu.addAction( tr( "Copy selected values with headers" ), &contextMenu, [this]
  {
    this->copyHydrographSelected( true );
  } );

  contextMenu.exec( ui->tableViewHydrographResult->mapToGlobal( pos ) );
}

void ReosRunoffHydrographWidget::rainfallRunoffTabContextMenu( const QPoint &pos )
{
  QMenu contextMenu;

  contextMenu.addAction( tr( "Copy selected values" ), &contextMenu, [this]
  {
    this->copyRainfallRunoffSelected( false );
  } );

  contextMenu.addAction( tr( "Copy selected values with headers" ), &contextMenu, [this]
  {
    this->copyRainfallRunoffSelected( true );
  } );

  contextMenu.exec( ui->tableViewRunoffResult->mapToGlobal( pos ) );
}

void ReosRunoffHydrographWidget::onTransferFunctionFormulation()
{
  if ( !ReosTransferFunctionFactories::isInstantiate() )
    return;

  ReosTransferFunctionFactories *factories = ReosTransferFunctionFactories::instance();

  int currentIndex = ui->comboBoxTransferFunction->currentIndex();

  if ( mCurrentWatershed && currentIndex >= 0 )
  {
    QString type = factories->type( currentIndex );
    QDialog *dial = new QDialog( this );

    QFont font = dial->font();
    font.setPointSizeF( 11 );
    dial->setFont( font );

    dial->setWindowTitle( ui->comboBoxTransferFunction->currentText() );
    dial->setLayout( new QVBoxLayout );
    QLabel *labelpresentation = new QLabel( dial );
    labelpresentation->setTextFormat( Qt::RichText );
    labelpresentation->setWordWrap( true );
    labelpresentation->setText( factories->presentationText( type ) );
    dial->layout()->addWidget( labelpresentation );

    QLabel *formulationLabel = new QLabel( this );
    formulationLabel->setAlignment( Qt::AlignHCenter );
    formulationLabel->setPixmap( factories->formulation( type ) );

    dial->layout()->addWidget( formulationLabel );
    QLabel *labelVariables = new QLabel( dial );
    labelVariables->setTextFormat( Qt::RichText );
    labelVariables->setWordWrap( true );
    labelVariables->setText( factories->variablesDescription( type ) );

    dial->layout()->addWidget( labelVariables );

    dial->exec();

    dial->deleteLater();
  }
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

ReosWatershedRunoffModelsModel::ReosWatershedRunoffModelsModel( QObject *parent ): QAbstractTableModel( parent ) {}

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
  form->addLine();
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
    lagTimeWidget->setVisible( !useConcTime );
    factorWidget->setVisible( useConcTime );
    lagTimeDeduced->setVisible( useConcTime );
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
  form->addLine();

  return form.release();
}

ReosTimeSeriesTableModel::ReosTimeSeriesTableModel( QObject *parent ): QAbstractTableModel( parent ) {}

QModelIndex ReosTimeSeriesTableModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSeriesTableModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTimeSeriesTableModel::rowCount( const QModelIndex & ) const
{
  int maxCount = 0;

  for ( int i = 0; i < mTimeSeries.count(); ++i )
    if ( !mTimeSeries.at( i ).isNull() )
      if ( maxCount < mTimeSeries.at( i )->valueCount() )
        maxCount = mTimeSeries.at( i )->valueCount();

  return maxCount;
}

int ReosTimeSeriesTableModel::columnCount( const QModelIndex & ) const
{
  if ( mTimeSeries.isEmpty() )
    return 0;

  return mTimeSeries.count() + 1;

}

QVariant ReosTimeSeriesTableModel::data( const QModelIndex &index, int role ) const
{
  if ( index.row() >= rowCount( QModelIndex() ) )
    return QVariant();

  if ( columnCount( QModelIndex() ) <= 0 )
    return QVariant();

  int row = index.row();

  if ( role == Qt::DisplayRole )
  {
    switch ( index.column() )
    {
      case 0: //time
        if ( !mTimeSeries.at( 0 ).isNull() && row < mTimeSeries.at( 0 )->valueCount() )
        {
          QDateTime time = mTimeSeries.at( 0 )->timeAt( row );
          time.setTimeSpec( Qt::UTC );
          return time.toString( "yyyy.MM.dd hh:mm:ss" );
        }
        break;
      default:
      {
        ReosTimeSerie *serie = mTimeSeries.at( index.column() - 1 );
        if ( !serie )
          return QVariant();
        if ( row >= serie->valueCount() )
          return QVariant();

        return serie->valueAt( row );
      }
      break;;
    }
  }

  if ( role == Qt::TextAlignmentRole )
  {
    return Qt::AlignHCenter;
  }

  return QVariant();
}

QVariant ReosTimeSeriesTableModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( section > mTimeSeries.count() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    switch ( section )
    {
      case 0: //time
        return tr( "Time" );
        break;
      default:
        return mHeaderName.at( section - 1 );
        break;
    }
  }

  return QVariant();
}

void ReosTimeSeriesTableModel::addTimeSerie( ReosTimeSerie *timeSerie, const QString &name )
{
  beginResetModel();
  mTimeSeries.append( timeSerie );
  mHeaderName.append( name );
  endResetModel();
}

void ReosTimeSeriesTableModel::clearSerie()
{
  beginResetModel();
  mTimeSeries.clear();
  mHeaderName.clear();
  endResetModel();
}

ReosFormWidget *ReosFormSCSUnithydrographWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTransferFunctionSCSUnitHydrograph *transferFunction = qobject_cast<ReosTransferFunctionSCSUnitHydrograph *>( dataObject );
  if ( !transferFunction )
    return nullptr;

  std::unique_ptr<ReosFormWidget> form = std::make_unique<ReosFormWidget>( parent );

  form->addParameter( transferFunction->area(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->concentrationTime(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addLine();
  form->addParameter( transferFunction->peakRateFactor(), -1, ReosParameterWidget::SpacerInMiddle );
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

  QObject::connect( transferFunction->useConcentrationTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, factorWidget, lagTimeWidget, lagTimeDeduced, lagTimeDeducedtext]
  {
    bool useConcTime = transferFunction->useConcentrationTime()->value();
    lagTimeWidget->setVisible( !useConcTime );
    factorWidget->setVisible( useConcTime );
    lagTimeDeduced->setVisible( useConcTime );
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

QModelIndex ReosTimeSeriesVariableTimeStepTabModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSeriesVariableTimeStepTabModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTimeSeriesVariableTimeStepTabModel::rowCount( const QModelIndex & ) const
{
  if ( mTimeSeries.isEmpty() )
    return 0;

  if ( isFixedTimeStep() )
  {
    return mTimeStepCount;
  }

  int maxCount = 0;

  for ( int i = 0; i < mTimeSeries.count(); ++i )
    if ( !mTimeSeries.at( i ).isNull() )
      if ( maxCount < mTimeSeries.at( i )->valueCount() )
        maxCount = mTimeSeries.at( i )->valueCount();

  return maxCount;
}

int ReosTimeSeriesVariableTimeStepTabModel::columnCount( const QModelIndex & ) const
{
  if ( mTimeSeries.isEmpty() )
    return 0;

  return mTimeSeries.count() + 1;
}

QVariant ReosTimeSeriesVariableTimeStepTabModel::data( const QModelIndex &index, int role ) const
{
  if ( index.row() >= rowCount( QModelIndex() ) )
    return QVariant();

  if ( columnCount( QModelIndex() ) <= 0 )
    return QVariant();

  int row = index.row();

  if ( role == Qt::DisplayRole )
  {
    switch ( index.column() )
    {
      case 0: //time
        return timeAtRow( row ).toString( "yyyy.MM.dd hh:mm:ss" );
        break;
      default:
        return valueAt( row, index.column() );
        break;;
    }
  }

  if ( role == Qt::TextAlignmentRole )
  {
    return Qt::AlignHCenter;
  }

  return QVariant();
}

QVariant ReosTimeSeriesVariableTimeStepTabModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( section > mTimeSeries.count() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    switch ( section )
    {
      case 0: //time
        return tr( "Time" );
        break;
      default:
        return mHeaderName.at( section - 1 );
        break;
    }
  }

  return QVariant();
}

void ReosTimeSeriesVariableTimeStepTabModel::addTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie, const QString &name )
{
  beginResetModel();
  mTimeSeries.append( timeSerie );
  mHeaderName.append( name );
  endResetModel();
  connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosTimeSeriesVariableTimeStepTabModel::updateTimeStep );
  updateTimeStep();
}

void ReosTimeSeriesVariableTimeStepTabModel::clearSerie()
{
  beginResetModel();
  for ( int i = 0; i < mTimeSeries.count(); i++ )
  {
    if ( mTimeSeries.at( i ).isNull() )
      continue;
    disconnect( mTimeSeries.at( i ).data(), &ReosDataObject::dataChanged, this, &ReosTimeSeriesVariableTimeStepTabModel::updateTimeStep );
  }
  mTimeSeries.clear();
  mHeaderName.clear();
  endResetModel();
}

void ReosTimeSeriesVariableTimeStepTabModel::updateTimeStep()
{
  if ( !isFixedTimeStep() )
    return;
  beginResetModel();

  QDateTime firstTime;
  QDateTime lastTime;

  for ( int i = 0; i < mTimeSeries.count(); ++i )
  {
    if ( mTimeSeries.at( i ).isNull() )
      continue;
    ReosTimeSerieVariableTimeStep *serie = mTimeSeries.at( i );
    if ( serie->valueCount() == 0 )
      continue;
    QDateTime begin = serie->timeAt( 0 );
    QDateTime end = serie->timeAt( serie->valueCount() - 1 );

    if ( !firstTime.isValid() || ( begin.isValid() && firstTime >= begin ) )
      firstTime = begin;

    if ( !lastTime.isValid() || ( end.isValid() && lastTime <= end ) )
      lastTime = end;
  }

  mFirstTime = firstTime;
  mTimeStepCount = ReosDuration( firstTime.msecsTo( lastTime ) ) / mTimeStep + 1;

  endResetModel();
}

ReosDuration ReosTimeSeriesVariableTimeStepTabModel::timeStep() const
{
  return mTimeStep;
}

void ReosTimeSeriesVariableTimeStepTabModel::setTimeStep( const ReosDuration &timeStep )
{
  beginResetModel();
  mTimeStep = timeStep;
  endResetModel();
  updateTimeStep();
}

QDateTime ReosTimeSeriesVariableTimeStepTabModel::timeAtRow( int row ) const
{
  if ( isFixedTimeStep() )
    return mFirstTime.addMSecs( ( mTimeStep * row ).valueMilliSecond() );

  if ( !mTimeSeries.at( 0 ).isNull() && row < mTimeSeries.at( 0 )->valueCount() )
    return mTimeSeries.at( 0 )->timeAt( row );

  return QDateTime();
}

QVariant ReosTimeSeriesVariableTimeStepTabModel::valueAt( int row, int column ) const
{
  ReosTimeSerieVariableTimeStep *serie = mTimeSeries.at( column - 1 );
  if ( !serie )
    return QVariant();

  if ( isFixedTimeStep() )
  {
    ReosDuration relativeTime( serie->referenceTime()->value().msecsTo( mFirstTime ) );
    return serie->valueAtTime( relativeTime + mTimeStep * row );
  }
  if ( row >= serie->valueCount() )
    return QVariant();

  return serie->valueAt( row );
}

bool ReosTimeSeriesVariableTimeStepTabModel::isFixedTimeStep() const
{
  return mIsFixedTimeStep || mTimeSeries.count() > 1;
}

void ReosTimeSeriesVariableTimeStepTabModel::setIsFixedTimeStep( bool isFixedTimeStep )
{
  beginResetModel();
  mIsFixedTimeStep = isFixedTimeStep;
  endResetModel();
  updateTimeStep();
}

ReosFormWidget *ReosFormNashUnithydrographWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTransferFunctionNashUnitHydrograph *transferFunction = qobject_cast<ReosTransferFunctionNashUnitHydrograph *>( dataObject );
  if ( !transferFunction )
    return nullptr;

  std::unique_ptr<ReosFormWidget> form = std::make_unique<ReosFormWidget>( parent );

  form->addParameter( transferFunction->area(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->concentrationTime(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addLine();
  form->addParameter( transferFunction->nParam(), -1, ReosParameterWidget::SpacerInMiddle );
  form->addParameter( transferFunction->useConcentrationTime(), -1, ReosParameterWidget::SpacerAfter );
  QWidget *kWidget = form->addParameter( transferFunction->KParam(), -1, ReosParameterWidget::SpacerInMiddle );
  QLabel *KDeducedLabel = new QLabel( form.get() );
  form->addWidget( KDeducedLabel );


  bool useConcTime = transferFunction->useConcentrationTime()->value();
  KDeducedLabel->setVisible( useConcTime );
  kWidget->setVisible( !useConcTime );

  QString KDeducedtext = QObject::tr( "K parameter from concentration time: %1" );
  ReosDuration concTime = transferFunction->concentrationTime()->value();
  ReosDuration deducedK = concTime /  transferFunction->nParam()->value();
  deducedK.setAdaptedUnit();
  KDeducedLabel->setText( KDeducedtext.arg( ( deducedK ).toString( 2 ) ) );

  QObject::connect( transferFunction->useConcentrationTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, kWidget, KDeducedLabel, KDeducedtext]
  {
    bool useConcTime = transferFunction->useConcentrationTime()->value();
    kWidget->setVisible( !useConcTime );
    KDeducedLabel->setVisible( useConcTime );

  } );

  QObject::connect( transferFunction->nParam(), &ReosParameter::valueChanged, form.get(), [transferFunction, KDeducedLabel, KDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    ReosDuration deducedK = concTime /  transferFunction->nParam()->value();
    deducedK.setAdaptedUnit();
    KDeducedLabel->setText( KDeducedtext.arg( ( deducedK ).toString( 2 ) + ' ' + deducedK.unitToString() ) );
  } );

  QObject::connect( transferFunction->concentrationTime(), &ReosParameter::valueChanged, form.get(), [transferFunction, KDeducedLabel, KDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    ReosDuration deducedK = concTime /  transferFunction->nParam()->value();
    deducedK.setAdaptedUnit();
    KDeducedLabel->setText( KDeducedtext.arg( ( deducedK ).toString( 2 ) ) );
  } );

  QObject::connect( transferFunction->concentrationTime(), &ReosParameter::unitChanged, form.get(), [transferFunction, KDeducedLabel, KDeducedtext]
  {
    ReosDuration concTime = transferFunction->concentrationTime()->value();
    ReosDuration deducedK = concTime /  transferFunction->nParam()->value();
    deducedK.setAdaptedUnit();
    KDeducedLabel->setText( KDeducedtext.arg( ( deducedK ).toString( 2 ) ) );
  } );

  return form.release();
}
