/***************************************************************************
  reosvariabletimesteptimeseriesgroupwidget.cpp - ReosVariableTimeStepTimeSeriesGroupWidget

 ---------------------
 begin                : 11.4.2022
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
#include "reosvariabletimesteptimeseriesgroupwidget.h"
#include "ui_reosvariabletimestepseriesgroupwidget.h"

#include <QMessageBox>

#include "reosguicontext.h"
#include "reosstyleregistery.h"
#include "reosformwidget.h"
#include "reostimeseries.h"
#include "reostimeseriesgroup.h"
#include "reosplottimeconstantinterval.h"
#include "reosdataprovidergui.h"
#include "reostableview.h"
#include "reosformwidget.h"
#include "reossettings.h"

ReosVariableTimeStepTimeSeriesGroupWidget::ReosVariableTimeStepTimeSeriesGroupWidget( const ReosGuiContext &guiContext, const QString genericName, const QString &unitString, int currentIndex )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosTimeSeriesVariableTimeStepWidget )
  , mMap( guiContext.map() )
  , mGenericSeriesName( genericName )
  , mUnitString( unitString )
{
  ui->setupUi( this );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosVariableTimeStepSeriesEditingWidgetFactory );

  ui->mCurrentTimeSeriesLabel->setText( tr( "Current %1" ).arg( mGenericSeriesName ) );
  ui->mWidgetToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mWidgetToolBar );
  toolBar->setIconSize( QSize( 16, 16 ) );
  toolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mWidgetToolBar->layout()->addWidget( toolBar );

  mActionAddSeries = toolBar->addAction( QIcon( QStringLiteral( ":/images/add.svg" ) ),
                                         tr( "Add %1" ).arg( mGenericSeriesName ), this, &ReosVariableTimeStepTimeSeriesGroupWidget::onAddSeries );
  mActionDeleteSeries = toolBar->addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ),
                        tr( "Delete %1" ).arg( mGenericSeriesName ), this, &ReosVariableTimeStepTimeSeriesGroupWidget::onRemoveSeries );
  mActionRenameSeries = toolBar->addAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ),
                        tr( "Rename %1" ).arg( mGenericSeriesName ), this, &ReosVariableTimeStepTimeSeriesGroupWidget::onRenameSeries );

  ui->mWidgetProviderToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetProviderToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  mToolBarProvider = new QToolBar( ui->mWidgetProviderToolBar );
  mToolBarProvider->setIconSize( QSize( 24, 24 ) );
  mToolBarProvider->layout()->setContentsMargins( 0, 0, 0, 0 );
  mToolBarProvider->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mWidgetProviderToolBar->layout()->addWidget( mToolBarProvider );

  mSeriesPlot = new ReosPlotTimeSerieVariableStep( mGenericSeriesName );
  ui->plotWidget->setSettingsContext( QStringLiteral( "time-series-%1" ) );
  ui->plotWidget->addPlotItem( mSeriesPlot );
  ui->plotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->plotWidget->enableAutoMinimumSize( true );
  ui->plotWidget->setLegendEnabled( false );
  ui->plotWidget->setTitleAxeYLeft( QStringLiteral( "%1, %2" ).arg( mGenericSeriesName, mUnitString ) );

  connect( ui->mComboBoxTimeSeriesName, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosVariableTimeStepTimeSeriesGroupWidget::onCurrentSeriesChanged );

  ui->mBackButton->setVisible( false );
  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  onGroupChanged();

  if ( currentIndex < ui->mComboBoxTimeSeriesName->count() )
    ui->mComboBoxTimeSeriesName->setCurrentIndex( currentIndex );
}

ReosVariableTimeStepTimeSeriesGroupWidget::~ReosVariableTimeStepTimeSeriesGroupWidget()
{
  delete ui;
}

void ReosVariableTimeStepTimeSeriesGroupWidget::setTimeSeriesGroup( ReosTimeSeriesVariableTimeStepGroup *group )
{
  if ( mGroup )
    disconnect( mGroup, &ReosDataObject::dataChanged, this, &ReosVariableTimeStepTimeSeriesGroupWidget::onGroupChanged );

  mGroup = group;

  if ( !mGroup )
  {
    mSeriesPlot->setTimeSeries( nullptr );
    setEnabled( false );
  }
  else
  {
    setEnabled( true );
    connect( mGroup, &ReosDataObject::dataChanged, this, &ReosVariableTimeStepTimeSeriesGroupWidget::onGroupChanged );
  }

  onGroupChanged();
  onCurrentSeriesChanged();
}

void ReosVariableTimeStepTimeSeriesGroupWidget::showBackButton()
{
  ui->mBackButton->setVisible( true );
}

void ReosVariableTimeStepTimeSeriesGroupWidget::onAddSeries()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Add %1" ).arg( mGenericSeriesName ) );
  dial->addText( tr( "Add a new %1, choose a name:" ).arg( mGenericSeriesName ) );
  ReosParameterString nameParam( tr( "%1 name" ).arg( mGenericSeriesName ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    std::unique_ptr<ReosTimeSeriesVariableTimeStep> newSeries = std::make_unique<ReosTimeSeriesVariableTimeStep>();
    newSeries->setName( nameParam.value() );
    newSeries->setColor( Qt::blue );
    newSeries->setReferenceTime( QDateTime( QDate::currentDate(), QTime( 0, 0, 0 ), Qt::UTC ) );
    mGroup->addTimeSeries( newSeries.release() );

    ui->mComboBoxTimeSeriesName->setCurrentIndex( ui->mComboBoxTimeSeriesName->count() - 1 );
  }

  dial->deleteLater();
}

void ReosVariableTimeStepTimeSeriesGroupWidget::onRemoveSeries()
{
  if ( !mGroup )
    return;

  int currentIndex = ui->mComboBoxTimeSeriesName->currentIndex();
  ReosTimeSeriesVariableTimeStep *timeSeriesToRemove = mGroup->timeSeries( currentIndex );
  if ( !timeSeriesToRemove )
    return;

  if ( QMessageBox::warning( this, tr( "Remove %1" ).arg( mGenericSeriesName ),
                             tr( "Do you want to remove the %1 '%2'?" ).arg( mGenericSeriesName, timeSeriesToRemove->name() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
  {
    mGroup->removeTimeSeries( currentIndex );
    ui->mComboBoxTimeSeriesName->removeItem( currentIndex );
  }

  onGroupChanged();
}

void ReosVariableTimeStepTimeSeriesGroupWidget::onRenameSeries()
{
  if ( !mGroup )
    return;

  int currentIndex = ui->mComboBoxTimeSeriesName->currentIndex();
  ReosTimeSeriesVariableTimeStep *timeSeriesToRename = mGroup->timeSeries( currentIndex );
  if ( !timeSeriesToRename )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename %1" ).arg( mGenericSeriesName ) );
  dial->addText( tr( "Rename %1 '%2',\nchoose a new name:" ).arg( mGenericSeriesName, timeSeriesToRename->name() ) );
  ReosParameterString nameParam( tr( "%1 name" ).arg( mGenericSeriesName ) );
  nameParam.setValue( timeSeriesToRename->name() );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    timeSeriesToRename->setName( nameParam.value() );
    ui->mComboBoxTimeSeriesName->setItemText( currentIndex, nameParam.value() );
  }

  dial->deleteLater();
}

void ReosVariableTimeStepTimeSeriesGroupWidget::onGroupChanged()
{
  ui->mComboBoxTimeSeriesName->clear();
  bool hasSeries = false;

  if ( mGroup )
  {
    const QStringList &names = mGroup->seriesNames();
    ui->mComboBoxTimeSeriesName->addItems( names );
    hasSeries = !names.isEmpty();
    mActionAddSeries->setEnabled( true );
  }
  else
  {
    mActionAddSeries->setEnabled( false );
  }

  mActionDeleteSeries->setEnabled( hasSeries );
  mActionRenameSeries->setEnabled( hasSeries );
}

void ReosVariableTimeStepTimeSeriesGroupWidget::onCurrentSeriesChanged()
{
  if ( !mCurrentSeries.isNull() )
  {
    disconnect( mCurrentSeries, &ReosDataObject::dataChanged, this, &ReosVariableTimeStepTimeSeriesGroupWidget::updatePlotExtent );
  }

  if ( mGroup && mGroup->timeSeriesCount() > 0 )
    mCurrentSeries = mGroup->timeSeries( ui->mComboBoxTimeSeriesName->currentIndex() );
  else
    mCurrentSeries = nullptr;

  std::unique_ptr<QWidget> newEditingWidget;
  std::unique_ptr<QWidget> newSettingsProviderWidget;

  if ( !mCurrentSeries.isNull() )
  {
    newSettingsProviderWidget.reset(
      ReosDataProviderGuiRegistery::instance()->createProviderSettingsWidget( mCurrentSeries->dataProvider() ) );
    newEditingWidget.reset( ReosFormWidgetFactories::instance()->createDataFormWidget( mCurrentSeries ) );
    connect( mCurrentSeries, &ReosDataObject::dataChanged, this, &ReosVariableTimeStepTimeSeriesGroupWidget::updatePlotExtent );
  }
  else
  {
    newEditingWidget.reset( new QLabel( tr( "No Hydrograph" ) ) );
  }

  mSeriesPlot->setTimeSeries( mCurrentSeries );
  ui->plotWidget->updatePlot();

  if ( ui->mEditingWidgetLayout->count() != 0 )
  {
    ui->mEditingWidgetLayout->removeWidget( mCurrenEditingWidget );
    delete mCurrenEditingWidget;
    ui->mEditingWidgetLayout->removeWidget( mCurrentProviderSettingsWidget );
    delete mCurrentProviderSettingsWidget;
  }

  mCurrentProviderSettingsWidget = newSettingsProviderWidget.release();
  if ( mCurrentProviderSettingsWidget )
    ui->mEditingWidgetLayout->addWidget( mCurrentProviderSettingsWidget );

  mCurrenEditingWidget = newEditingWidget.release();
  ui->mEditingWidgetLayout->addWidget( mCurrenEditingWidget );

  ui->mEditingWidgetLayout->setStretch( ui->mEditingWidgetLayout->count() - 1, 1 );
}

void ReosVariableTimeStepTimeSeriesGroupWidget::updatePlotExtent()
{

}

ReosVariableTimeStepSeriesEditingWidget::ReosVariableTimeStepSeriesEditingWidget( ReosTimeSeriesVariableTimeStep *timeSeries, QWidget *parent )
  : ReosFormWidget( parent, Qt::Vertical, false )
  , mIsUseConstantTimeStepForNewEntry( new ReosParameterBoolean( tr( "Use constant time step for new entry" ), false, this ) )
  , mConstantTimeStepForNewEntry( new ReosParameterDuration( tr( "Constant time step" ) ) )
{
  ReosSettings settings;

  mDataModel = qobject_cast<ReosTimeSeriesVariableTimeStepModel *>( timeSeries->model() );

  mIsUseConstantTimeStepForNewEntry->setValue( false );
  addParameter( timeSeries->referenceTimeParameter() );
  if ( timeSeries->dataProvider()->isEditable() )
  {
    addParameter( mIsUseConstantTimeStepForNewEntry );

    ReosDuration newEntryFixedTimeStep( 5, ReosDuration::minute );
    if ( settings.contains( QStringLiteral( "/time-series/new-entry-time-step-value" ) ) &&
         settings.contains( QStringLiteral( "/time-series/new-entry-time-step-unit" ) ) )
    {
      double timeStepValue = settings.value( QStringLiteral( "/time-series/new-entry-time-step-value" ) ).toDouble();
      ReosDuration::Unit unit = static_cast<ReosDuration::Unit>(
                                  settings.value( QStringLiteral( "/time-series/new-entry-time-step-value" ) ).toInt() );

      newEntryFixedTimeStep = ReosDuration( timeStepValue, unit );
    }
    mConstantTimeStepForNewEntry->setValue( newEntryFixedTimeStep );



    if ( settings.contains( QStringLiteral( "/time-series/new-entry-use-constant-time-step" ) ) )
    {
      mIsUseConstantTimeStepForNewEntry->setValue( settings.value( QStringLiteral( "/time-series/new-entry-use-constant-time-step" ) ).toBool() );
    }

    ReosDuration::Unit timeStepUnit = ReosDuration::minute;
    if ( settings.contains( QStringLiteral( "/time-series/time-step-unit" ) ) )
    {
      timeStepUnit = static_cast<ReosDuration::Unit>(
                       settings.value( QStringLiteral( "/time-series/time-step-unit" ) ).toInt() );
    }

    QWidget *relativeTimeUnitWidget = new QWidget( this );
    relativeTimeUnitWidget->setLayout( new QHBoxLayout );
    relativeTimeUnitWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
    relativeTimeUnitWidget->layout()->addWidget( new QLabel( tr( "Time step unit" ) ) );
    mTimeStepUnitCombo = new ReosDurationUnitComboBox( this, timeStepUnit );
    relativeTimeUnitWidget->layout()->addWidget( mTimeStepUnitCombo );
    addWidget( relativeTimeUnitWidget );
    mConstantTimeStepForNewEntryWidget = addParameter( mConstantTimeStepForNewEntry );
    mConstantTimeStepForNewEntryWidget->setVisible( mIsUseConstantTimeStepForNewEntry->value() );
    mDataModel->setNewRowWithFixedTimeStep( mIsUseConstantTimeStepForNewEntry->value() );

    connect( mIsUseConstantTimeStepForNewEntry, &ReosParameter::valueChanged, this, [this, relativeTimeUnitWidget]
    {
      bool useConstantTimeStep = mIsUseConstantTimeStepForNewEntry->value();
      ReosSettings settings;
      settings.setValue( QStringLiteral( "/time-series/new-entry-use-constant-time-step" ), useConstantTimeStep );
      mConstantTimeStepForNewEntryWidget->setVisible( useConstantTimeStep );
      mDataModel->setNewRowWithFixedTimeStep( useConstantTimeStep );
      relativeTimeUnitWidget->setVisible( !useConstantTimeStep );
    } );

    connect( mConstantTimeStepForNewEntry, &ReosParameter::valueChanged, this, [this]
    {
      mDataModel->setFixedTimeStep( mConstantTimeStepForNewEntry->value() );
      ReosSettings settings;
      settings.value( QStringLiteral( "/time-series/new-entry-time-step-value" ), mConstantTimeStepForNewEntry->value().valueUnit() );
      settings.value( QStringLiteral( "/time-series/new-entry-time-step-unit" ), mConstantTimeStepForNewEntry->value().unit() );
    } );

    connect( mTimeStepUnitCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
    {
      ReosDuration::Unit unit = mTimeStepUnitCombo->currentUnit();
      mDataModel->setVariableTimeStepUnit( unit );
      ReosSettings settings;
      settings.value( QStringLiteral( "/time-series/time-step-unit" ), unit );
    } );

  }

  ReosTimeSerieTableView *tableView = new ReosTimeSerieTableView( this );
  addWidget( tableView );
  tableView->setModel( mDataModel );
  tableView->verticalHeader()->hide();
}

ReosVariableTimeStepSeriesEditingWidget::~ReosVariableTimeStepSeriesEditingWidget()
{

}

QString ReosVariableTimeStepSeriesEditingWidgetFactory::datatype() const
{
  return ReosTimeSeriesVariableTimeStep::staticType();
}

ReosFormWidget *ReosVariableTimeStepSeriesEditingWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosTimeSeriesVariableTimeStep *ts = qobject_cast<ReosTimeSeriesVariableTimeStep *>( dataObject );
  if ( ts )
    return new ReosVariableTimeStepSeriesEditingWidget( ts, context.parent() );
  else
    return nullptr;
}
