/***************************************************************************
  reosrainfallintensitydurationwidget.h - ReosRainfallIntensityDurationWidget

 ---------------------
 begin                : 4.2.2021
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
#ifndef REOSRAINFALLINTENSITYDURATIONWIDGET_H
#define REOSRAINFALLINTENSITYDURATIONWIDGET_H

#include "reosformwidget.h"

class ReosIntensityDurationCurve;
class ReosIntensityDurationCurveTableModel;
class QComboBox;


class ReosRainfallIntensityDurationWidget: public ReosFormWidget
{
    Q_OBJECT
  public:
    explicit ReosRainfallIntensityDurationWidget( ReosIntensityDurationCurve *curve, QWidget *parent );

  private slots:
    void onVerticalHeaderDoubleClicked( int section );
    void onTableViewContextMenu( const QPoint &pos );
    void onVerticalHeaderViewContextMenu( const QPoint &pos );

  private:
    QComboBox *mComboFormula = nullptr;
    ReosDurationUnitComboBox *mTimeUnitComboBox = nullptr;
    ReosIntensityDurationCurveTableModel *mModel = nullptr;
    QTableView *mView = nullptr;

    void contextMenu( const QPoint &globalPos, const QModelIndex &index );
};

#endif // REOSRAINFALLINTENSITYDURATIONWIDGET_H
