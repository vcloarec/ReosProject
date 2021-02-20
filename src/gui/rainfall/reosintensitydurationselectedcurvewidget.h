/***************************************************************************
  reosintensitydurationselectedcurvewidget.h - ReosIntensityDurationSelectedCurveWidget

 ---------------------
 begin                : 10.2.2021
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
#ifndef REOSINTENSITYDURATIONSELECTEDCURVEWIDGET_H
#define REOSINTENSITYDURATIONSELECTEDCURVEWIDGET_H

#include <QWidget>
#include <QDialog>

#include "reosrainfallitem.h"

class QTreeView;
class QLabel;
class QDialogButtonBox;

class ReosRainfallModel;

namespace Ui
{
  class ReosIntensityDurationSelectedCurveWidget;
}

class ReosIntensityDurationSelectedCurveWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosIntensityDurationSelectedCurveWidget( QWidget *parent = nullptr );
    ~ReosIntensityDurationSelectedCurveWidget();

    ReosRainfallIntensityDurationCurveItem *curveItem() const;

    void setCurveItem( ReosRainfallIntensityDurationCurveItem *curve );
    void clearCurveItem();

    void setTitle( const QString &title );

  public slots:
    void onCurveChanged();

  private slots:
    void onToolButtonCurve();

  signals:
    void curveChanged( ReosRainfallIntensityDurationCurveItem *newCurve );

  private:
    Ui::ReosIntensityDurationSelectedCurveWidget *ui;
    QPointer<ReosRainfallIntensityDurationCurveItem> mCurveItem = nullptr;
};

class ReosRainfallItemSelectionDialog: public QDialog
{
  public:
    Q_OBJECT
  public:
    explicit ReosRainfallItemSelectionDialog( QWidget *parent = nullptr );
    void setSelectionType( ReosRainfallItem::Type type, QString dataType = QString() );
    void setText( const QString &text );
    ReosRainfallItem *selectedItem() const;

  private slots:
    void onSelectionChange();

  private:
    QTreeView *mTreeView;
    ReosRainfallModel *mModel = nullptr;
    ReosRainfallItem::Type mSelectionType = ReosRainfallItem::Zone;
    QString  mSelectionDataType;
    QLabel *mTextLabel;
    QDialogButtonBox *mButtonBox = nullptr;
};

#endif // REOSINTENSITYDURATIONSELECTEDCURVEWIDGET_H
