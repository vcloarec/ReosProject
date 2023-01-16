/***************************************************************************
  reosgriddedrainfallselectorwidget.h - ReosGriddedRainfallSelectorWidget

 ---------------------
 begin                : 22.12.2022
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
#ifndef REOSGRIDDEDRAINFALLSELECTORWIDGET_H
#define REOSGRIDDEDRAINFALLSELECTORWIDGET_H

#include <QWidget>

#include "reosdataprovidergui.h"
#include "reosgriddedrainfallprovider.h"
#include "reosguicontext.h"
#include "reosgriddedrainitem.h"

class ReosMapPolygon;
class ReosDataProviderSelectorWidget;

namespace Ui
{
  class ReosGriddedRainfallSelectorWidget;
}

class REOSGUI_EXPORT ReosGriddedRainDataProviderSelectorWidget : public ReosDataProviderSelectorWidget
{
    Q_OBJECT
  public:
    ReosGriddedRainDataProviderSelectorWidget( QWidget *parent = nullptr ) : ReosDataProviderSelectorWidget( parent ) {}

    ReosGriddedRainfall *createData( QObject *parent = nullptr )  const override;

    //! Set the datasource \a source to populate the selector, and returns details. Default implementation does nothing
    virtual ReosGriddedRainfallProvider::Details setSource( const QString &source, ReosModule::Message &message ) {return ReosGriddedRainfallProvider::Details();};
};

//! Widget class used to select/defined gridded rainfall
class ReosGriddedRainfallSelectorWidget : public ReosDataProviderSelectorWidget
{
    Q_OBJECT

  public:
    explicit ReosGriddedRainfallSelectorWidget( const ReosGuiContext &guiContext );
    ~ReosGriddedRainfallSelectorWidget();

    ReosDataObject *createData( QObject *parent = nullptr ) const override;
    QVariantMap selectedMetadata() const override;

  private slots:
    void onPathButtonClicked();
    void onPathChanged();
    void updateRainfall();
    void updateDataOnMap();

  private:
    Ui::ReosGriddedRainfallSelectorWidget *ui;
    ReosGuiContext mGuiContext;
    std::unique_ptr<ReosMapPolygon> mDataExtent;
    QString mCurrentVariable;
    ReosGriddedRainfallProvider::Details mDetails;
    std::unique_ptr<ReosGriddedRainfall> mCurrentRainfall;
    std::unique_ptr<ReosGriddedRainfallProvider> mProvider;
    std::unique_ptr<ReosMapPolygon> mExtentOnMainMap;

    ReosGriddedRainDataProviderSelectorWidget *mProviderSelectorWidget = nullptr;

    bool mCurrentSourceIsValid = false;
    QString mCurrentDataUri;

    QString giveName() const;
};

#endif // REOSGRIDDEDRAINFALLSELECTORWIDGET_H
