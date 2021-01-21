/***************************************************************************
  reoschartwidget.cpp - ReosChartWidget

 ---------------------
 begin                : 13.1.2021
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
#include "reoschartwidget.h"
#include "ui_reoschartwidget.h"

#include<QToolButton>
#include <QPixmap>
#include <QtCharts/QAbstractSeries>

ReosChartWidget::ReosChartWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosChartWidget )
{
  ui->setupUi( this );


}

ReosChartWidget::~ReosChartWidget()
{
  delete ui;
}

void ReosChartWidget::setUniqueXYSeries( const QPolygonF &xySerie )
{
  ui->mChartView->setUniqueXYSeries( xySerie );
}

void ReosChartWidget::addCustomedSeries( QtCharts::QAbstractSeries *series )
{
  ui->mChartView->chart()->addSeries( series );
  ui->mChartView->chart()->createDefaultAxes();
}
