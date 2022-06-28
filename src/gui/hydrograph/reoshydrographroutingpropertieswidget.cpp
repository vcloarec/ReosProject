/***************************************************************************
  reoshydrographpropertieswidget.cpp - ReosHydrographPropertiesWidget

 ---------------------
 begin                : 28.5.2021
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
#include "reoshydrographroutingpropertieswidget.h"
#include "ui_reoshydrographroutingpropertieswidget.h"

#include "reoshydrographrouting.h"
#include "reosformwidget.h"
#include "reosplottimeconstantinterval.h"
#include "reoshydrograph.h"
#include "reosplotitemlist.h"
#include "reosapplication.h"
#include "reosguicontext.h"

ReosHydrographRoutingPropertiesWidget::ReosHydrographRoutingPropertiesWidget( ReosHydrographRoutingLink *hydrographRouting, QWidget *parent )
  :  ReosHydraulicElementWidget( parent )
  ,  ui( new Ui::ReosHydrographRoutingPropertiesWidget )
  , mRouting( hydrographRouting )
{
  ui->setupUi( this );

  QString settingsString = QStringLiteral( "hydraulic-network-rounting-link" );
  ui->mPlotsWidget->setSettingsContext( settingsString );

  ui->mPlotsWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotsWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotsWidget->enableAxeYRight( false );
  ui->mPlotsWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotsWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  mHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Hydrographs" ), ui->mPlotsWidget );
  ReosSettings settings;
  if ( settings.contains( settingsString ) )
    mHydrographPlotButton->setChecked( settings.value( settingsString ).toBool() );
  else
    mHydrographPlotButton->setChecked( true );

  mProgressControler = new ReosHydrauylicNetworkElementCalculationControler( hydrographRouting, this );
  mProgressControler->setProgressBar( ui->mProgressBar );

  const QStringList availableTypes = ReosHydrographRoutingMethodFactories::instance()->methodTypes();

  for ( const QString &type : availableTypes )
    ui->mRoutingTypeCombo->addItem( ReosHydrographRoutingMethodFactories::instance()->displayName( type ), type );

  ReosHydrographRoutingMethod *method = hydrographRouting->currentRoutingMethod();
  if ( method )
  {
    const QString currentMethodeType = method->type();
    ui->mRoutingTypeCombo->setCurrentIndex( ui->mRoutingTypeCombo->findData( currentMethodeType ) );
    mRoutingWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( method );
  }

  if ( mRoutingWidget )
    ui->mRoutingParametersWidget->layout()->addWidget( mRoutingWidget );

  connect( ui->mRoutingTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ),
           this, &ReosHydrographRoutingPropertiesWidget::onCurrentMethodChange );

  if ( settings.contains( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ) ) )
  {
    if ( settings.value( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ) ).toBool() )
      ui->mTabWidget->setCurrentIndex( 1 );
    else
      ui->mTabWidget->setCurrentIndex( 0 );
  }

  connect( ui->mTabWidget, &QTabWidget::currentChanged, this, [this]
  {
    ReosSettings settings;
    settings.setValue( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ), ui->mTabWidget->currentIndex() == 1 );
  } );

  ui->mTablesWidget->setConstantTimeStepParameter( hydrographRouting->constantTimeStepInTable(), hydrographRouting->useConstantTimeStepInTable() );
  populateHydrographs();

  connect( mRouting, &ReosHydrographJunction::calculationIsUpdated, this, &ReosHydrographRoutingPropertiesWidget::updateInformation );
  ui->mNotificationButton->setVisible( false );
  updateInformation();

  connect( ui->mRoutingDescriptionButton, &QPushButton::clicked, this, &ReosHydrographRoutingPropertiesWidget::onMethodDescription );
  connect( mRouting, &ReosDataObject::dataChanged, this, &ReosHydrographRoutingPropertiesWidget::syncToLink );
}

ReosHydrographRoutingPropertiesWidget::~ReosHydrographRoutingPropertiesWidget()
{
  delete ui;
}

void ReosHydrographRoutingPropertiesWidget::setCurrentCalculationContext( const ReosCalculationContext &context )
{
  if ( mRouting )
    mRouting->updateCalculationContext( context );
}

void ReosHydrographRoutingPropertiesWidget::onCurrentMethodChange()
{
  QString newType = ui->mRoutingTypeCombo->currentData().toString();
  mRouting->setCurrentRoutingMethod( newType );

  onMethodChange();
}

void ReosHydrographRoutingPropertiesWidget::updateInformation()
{
  if ( ! mRouting->outputHydrograph() || mRouting->outputHydrograph()->valueCount() == 0 )
  {
    ui->mLabelPeak->setText( tr( "none" ) );
    ui->mLabelValueCount->setText( QLocale().toString( 0 ) );
  }
  else
  {
    ui->mLabelPeak->setText( QStringLiteral( "%1 %2" ).arg( QLocale().toString( mRouting->outputHydrograph()->maximum() ),
                             QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
    ui->mLabelValueCount->setText( QLocale().toString( mRouting->outputHydrograph()->valueCount() ) );
  }

  const ReosModule::Message notification = mRouting->lastMessage();
  switch ( notification.type )
  {
    case ReosModule::Simple:
    case ReosModule::Order:
      ui->mNotificationButton->setVisible( false );
      break;
    case ReosModule::Warning:
    case ReosModule::Error:
      ui->mNotificationButton->setVisible( true );
      break;
  }

  ui->mNotificationButton->setMessage( notification );
}

void ReosHydrographRoutingPropertiesWidget::onMethodDescription()
{
  ReosHydrographRoutingMethodFactories *factories = ReosHydrographRoutingMethodFactories::instance();

  if ( mRouting )
  {
    QString type = ui->mRoutingTypeCombo->currentData().toString();
    if ( type.isEmpty() )
      return;

    QDialog *dial = new QDialog( this );

    dial->setAttribute( Qt::WA_DeleteOnClose );
    dial->setModal( false );

    QTextBrowser *textBrowser = new QTextBrowser( dial );

    dial->setWindowTitle( ui->mRoutingTypeCombo->currentText() );
    dial->setLayout( new QVBoxLayout );
    dial->layout()->addWidget( textBrowser );

    textBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
    textBrowser->setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
    textBrowser->setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );

    textBrowser->setText( factories->htmlDescription( type ) );

    dial->show();

    textBrowser->document()->setTextWidth( 400 );
    dial->resize( textBrowser->document()->size().width() + dial->contentsMargins().left() + dial->contentsMargins().right() + 20,
                  textBrowser->document()->size().height() + dial->contentsMargins().top() + dial->contentsMargins().bottom() + 50 );
  }
}

void ReosHydrographRoutingPropertiesWidget::syncToLink()
{
  QString methodType;
  if ( mRouting->currentRoutingMethod() )
    methodType = mRouting->currentRoutingMethod()->type();

  if ( ui->mRoutingTypeCombo->currentData().toString() != methodType )
  {
    ui->mRoutingTypeCombo->setCurrentIndex( ui->mRoutingTypeCombo->findData( methodType ) );
    onMethodChange();
  }
}

void ReosHydrographRoutingPropertiesWidget::populateHydrographs()
{
  ui->mTablesWidget->clearSeries();

  QList<QPointer<ReosHydrograph>> hydrographs;

  hydrographs.append( mRouting->inputHydrograph() );
  hydrographs.append( mRouting->outputHydrograph() );

  mHydrographPlotButton->addData( mRouting->inputHydrograph() );
  mHydrographPlotButton->addData( mRouting->outputHydrograph() );

  QList<ReosTimeSerieVariableTimeStep *> tsList;

  for ( ReosHydrograph *hyd : std::as_const( hydrographs ) )
    tsList.append( hyd );

  ui->mTablesWidget->setSeries( tsList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );
}

void ReosHydrographRoutingPropertiesWidget::onMethodChange()
{
  QWidget *newWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( mRouting->currentRoutingMethod() );

  if ( newWidget )
  {
    if ( mRoutingWidget )
      ui->mRoutingParametersWidget->layout()->replaceWidget( mRoutingWidget, newWidget );
    else
      ui->mRoutingParametersWidget->layout()->addWidget( newWidget );
  }
  else
    ui->mRoutingParametersWidget->layout()->removeWidget( mRoutingWidget );

  delete mRoutingWidget;
  mRoutingWidget = newWidget;
  mRouting->calculateRouting();
}

ReosHydraulicElementWidget *ReosHydrographRoutingPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  if ( !element )
    return nullptr;

  if ( !element->type().contains( ReosHydrographRoutingLink::staticType() ) )
    return nullptr;

  ReosHydrographRoutingLink *routing = qobject_cast<ReosHydrographRoutingLink *>( element );

  if ( !routing )
    return nullptr;

  return new ReosHydrographRoutingPropertiesWidget( routing, context.parent() );
}

QString ReosHydrographRoutingPropertiesWidgetFactory::elementType() {return ReosHydrographRoutingLink::staticType();}

ReosFormWidget *ReosFormHydrographRountingMuskingumWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydrographRoutingMethodMuskingum *routing = qobject_cast<ReosHydrographRoutingMethodMuskingum *>( dataObject );
  if ( !routing )
    return nullptr;

  ReosFormWidget *form = new ReosFormWidget( context.parent() );
  form->addParameter( routing->kParameter() );
  form->addParameter( routing->xParameter() );

  return form;
}

QString ReosFormHydrographRountingMuskingumWidgetFactory::datatype() const {return ReosHydrographRoutingMethodMuskingum::staticType();}

ReosFormWidget *ReosFormHydrographRountingLagWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydrographRoutingMethodLag *routing = qobject_cast<ReosHydrographRoutingMethodLag *>( dataObject );
  if ( !routing )
    return nullptr;

  ReosFormWidget *form = new ReosFormWidget( context.parent() );
  form->addParameter( routing->lagParameter() );

  return form;
}

QString ReosFormHydrographRountingLagWidgetFactory::datatype() const
{
  return ReosHydrographRoutingMethodLag::staticType();
}
