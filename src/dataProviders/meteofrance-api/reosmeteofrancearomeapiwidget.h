/***************************************************************************
  reosmeteofrancearomeapiwidget.h - ReosMeteoFranceAromeApiWidget

 ---------------------
 begin                : 3.4.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMETEOFRANCEAROMEAPIWIDGET_H
#define REOSMETEOFRANCEAROMEAPIWIDGET_H

#include "reosgriddedrainfallselectorwidget.h"
#include "reosmapitem.h"

class ReosMeteoFranceApiArome;
class ReosMapToolDrawExtent;

namespace Ui
{
  class ReosMeteoFranceAromeApiWidget;
}

class ReosMeteoFranceAromeApiWidget :  public ReosGriddedRainDataProviderSelectorWidget
{
    Q_OBJECT

  public:
    explicit ReosMeteoFranceAromeApiWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosMeteoFranceAromeApiWidget();

    ReosGriddedRainfall *createData( QObject *parent ) const override;
    QString dataName() const override;
    QVariantMap selectedMetadata() const override;

  private slots:
    void onApiKeyFileButton();
    void onConnectButton();
    void onZoneChange();
    void resetModel();
    void onExtentDrawn( const QRectF &extent );
    void onCurrentRainfallChanged();
    void onLoadingFinished();

  private:
    Ui::ReosMeteoFranceAromeApiWidget *ui;
    ReosMap *mMap = nullptr;
    std::unique_ptr<ReosMeteoFranceApiArome> mApi;
    bool mDataIsValid = false;
    int mRunFrameCount = 0;
    ReosMapExtent mExtent;
    ReosMapToolDrawExtent *mMapToolDrawExtent = nullptr;
    QAction *mActionDrawExtent = nullptr;

    QString mApiKey;
    QMap<QString, QStringList> mModels;
    mutable QPointer<ReosGriddedRainfall> mCurrentRainfall;

    void setExtent( const ReosMapExtent &extent, bool zoom );
    bool testKey();
};


class  ReosMeteoFranceAromeApiGuiFactory : public ReosDataProviderGuiFactory
{
  public:
    GuiCapabilities capabilities() const override;
    QString key() const override;
    ReosMeteoFranceAromeApiWidget *createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent = nullptr ) const override;
    ReosDataProviderSettingsWidget *createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr ) const override;
    QString dataType() const override;

    QString displayText() const override;
};

#endif // REOSMETEOFRANCEAROMEAPIWIDGET_H
