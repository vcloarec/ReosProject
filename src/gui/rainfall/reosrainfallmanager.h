/***************************************************************************
  reosrainfallmanager.h - ReosRainfallManager

 ---------------------
 begin                : 24.1.2021
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
#ifndef REOSRAINFALLMANAGER_H
#define REOSRAINFALLMANAGER_H

#include <QWidget>
#include <QDialog>
#include "reosactionwidget.h"
#include "reosrainfallitem.h"

namespace Ui
{
  class ReosRainfallManager;
}

class QAction;
class ReosRainfallModel;
class ReosRainfallDataItem;
class ReosStationItem;
class ReosFormWidget;
class ReosPlotWidget;


//! Widget to handle rainfall data
class ReosRainfallManager : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosRainfallManager( ReosRainfallModel *rainfallmodel, QWidget *parent = nullptr );
    ~ReosRainfallManager();

    //! Loads the data file defined on settings
    void loadDataFile();

  private slots:
    void onOpenRainfallFile();
    void onSaveRainfallFile();
    void onSaveAsRainfallFile();
    void onAddRootZone();
    void onAddZoneToZone();
    void onAddStation();
    void onAddGaugedRainfall();
    void onAddIDFCurves();
    void onAddIDCurve();
    void onReorderIDCurve();
    void onRemoveItem();
    void onCurrentTreeIndexChanged();
    void onTreeViewContextMenu( const QPoint &pos );

    void onImportFromTextFile();

  private:
    Ui::ReosRainfallManager *ui;
    ReosRainfallModel *mModel = nullptr;
    QString mCurrentFileName;
    QAction *mAction;

    QAction *mActionOpenRainfallDataFile = nullptr;
    QAction *mActionSaveRainfallDataFile = nullptr;
    QAction *mActionSaveAsRainfallDataFile = nullptr;
    QAction *mActionAddRootZone = nullptr;
    QAction *mActionAddZoneToZone = nullptr;
    QAction *mActionAddStation = nullptr;
    QAction *mActionAddGaugedRainfall = nullptr;
    QAction *mActionAddIDFCurves = nullptr;
    QAction *mActionAddIDCurve = nullptr;
    QAction *mActionReorderIdVurve = nullptr;
    QAction *mActionRemoveItem = nullptr;

    QAction *mActionImportFromTextFile = nullptr;

    ReosFormWidget *mCurrentForm = nullptr;
    ReosPlotWidget *mCurrentPlot = nullptr;

    void selectItem( ReosRainfallItem *item );
    bool saveOnFile( const QString &fileName );

    QList<QAction *> dataItemActions( ReosRainfallDataItem *dataItem );

    bool addSimpleItemDialog( const QString &title, QString &name, QString &description );
};

class ReosTextFileData;
class QComboBox;
class QDialogButtonBox;
class QLabel;
class QToolButton;
class QTreeView;
class ReosTimeSerieConstantInterval;

class ReosImportRainfallDialog: public QDialog
{
    Q_OBJECT
  public:
    explicit ReosImportRainfallDialog( ReosRainfallModel *model, QWidget *parent = nullptr );

  private slots:
    void onImportButton();
    void onSelectStationButton();

  private:
    ReosRainfallModel *mModel = nullptr;
    ReosTextFileData *mTextFile = nullptr;
    QComboBox *mComboSelectedField = nullptr;
    ReosTimeSerieConstantInterval *mImportedRainfall = nullptr;
    QToolButton *mImportButton = nullptr;
    QToolButton *mSelectStationButton = nullptr;
    ReosParameterString *mName = nullptr;
    ReosParameterString *mDescription = nullptr;
};


class ReosRainfallItemSelectionDialog: public QDialog
{
  public:
    Q_OBJECT
  public:
    explicit ReosRainfallItemSelectionDialog( ReosRainfallModel *model, QWidget *parent = nullptr );
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

#endif // REOSRAINFALLMANAGER_H
