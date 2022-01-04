/***************************************************************************
  reosdataprovidergui.h

 ---------------------
 begin                : 6.11.2021
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
#ifndef REOSDATAPROVIDERGUI_H
#define REOSDATAPROVIDERGUI_H

#include <memory>
#include <map>
#include <QWidget>

#include <reosgui.h>

class ReosMap;
class ReosDataObject;
class ReosDataProvider;

class REOSGUI_EXPORT ReosDataProviderSelectorWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosDataProviderSelectorWidget( QWidget *parent = nullptr ): QWidget( parent ) {}

    /**
     * Creates an return a pointer to a new data object with the current selected data,
     * \a parent take ownership of the object, or the caller need to take it if \a parent is nullptr
     * Default implementation return a null pointer
     */
    virtual ReosDataObject *createData( QObject *parent = nullptr ) const;

    //! Returns a pointer to the current selected object, default implementation return a null pointer.
    virtual ReosDataObject *selectedData() const;

    /**
     * Returns metadata of the data, must contains map following the Reos convention of the the datatype instead the provider convention:
     *
     * provider-key: provider key
     * station: if the data is associated with a station, the name of the station
     * station-descritpion: a short text desciption of the station
     * x-coord: x coordinate (or longitude)
     * y-coord: y coordinate (or longitude)
     * crs: wkt coordinate system
     * descritpion: a short text desciption of the data
     * start: start date/time of data if temporal
     * end:: end date/time of data if temporal
     */
    virtual QVariantMap selectedMetadata() const;

  signals:
    void dataSelectionChanged( bool dataIsSelected );
    void dataIsLoading();
    void dataIsReady();

  public slots:
    virtual void onClosed() {};
    virtual void onOpened() {};
};

class REOSGUI_EXPORT ReosDataProviderSettingsWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosDataProviderSettingsWidget( QWidget *parent = nullptr );
};

class REOSGUI_EXPORT ReosDataProviderGuiFactory
{
    Q_GADGET
  public:
    enum class GuiCapability
    {
      DataSelector = 1 << 0, //!< If the provider have a gui data selector
      ProviderSettings = 1 << 1, //!< If the providr have a settings widget
      StationIdentification = 1 << 2, //! If the provider can identify station
    };

    Q_ENUM( GuiCapability )
    Q_DECLARE_FLAGS( GuiCapabilities, GuiCapability )
    Q_FLAG( GuiCapabilities )

    /**
     * Returns the gui capabilities for this factory
     */
    virtual GuiCapabilities capabilities() const = 0;

    //! Returns the key of the provider corrsponding to this factory
    virtual QString key() const = 0;

    //! Creates and returns a pointer to a new selector widget
    virtual ReosDataProviderSelectorWidget *createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent = nullptr ) const;

    //! Creates and returns a pointer to a new data provider settings widget for the provider \a dataProvider
    virtual ReosDataProviderSettingsWidget *createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr ) const;

    //! Returns the data type string that this factory handle, used to retrieve all the factory of the same type (e.g. "hydrograph",...)
    virtual QString dataType() const = 0;

    //! Returns an icon representing this data provider
    virtual QPixmap icon() const;

    //! Returns a text representing this data provider
    virtual QString displayText() const {return QString();}
};

/**
 * Class that stores time serie provider factory
 */
class REOSGUI_EXPORT ReosDataProviderGuiRegistery
{
  public:
    ReosDataProviderGuiRegistery();

    //! Registers a data provider gui factory
    void registerProviderGuiFactory( ReosDataProviderGuiFactory *factory );

    //! Returns all the data provider keys of type \a dataType that have a gui that have \a capability
    QStringList providers( const QString &dataType, ReosDataProviderGuiFactory::GuiCapability capability ) const;

    //! Creates and returns a pointer to a new selector widget for the provider with \a key
    ReosDataProviderSelectorWidget *createProviderSelectorWidget( const QString &key, const QString &dataType, ReosMap *map, QWidget *parent = nullptr );

    //! Creates and returns a pointer to a new settings widget for the data provider \a dataProvider
    ReosDataProviderSettingsWidget *createProviderSettingsWidget( ReosDataProvider *dataProvider, QWidget *parent = nullptr );

    bool hasCapability( QString providerKey, ReosDataProviderGuiFactory::GuiCapability capability ) const;

    //! Returns the icon that represens the provider with the \a key
    QPixmap providerIcon( const QString &key ) const;

    //! Returns a text representing this data provider with \a key
    QString providerDisplayText( const QString &key ) const;

    //! Returns the static instance of this registery
    static ReosDataProviderGuiRegistery *instance();

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosDataProviderGuiFactory> dummy; // work arround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif

    std::map<QString, std::unique_ptr<ReosDataProviderGuiFactory>> mFactories;
    static ReosDataProviderGuiRegistery *sInstance;

    //! Returns the gui factory of the provider with \a key
    ReosDataProviderGuiFactory *guiFactory( const QString &key ) const;

    void loadDynamicProvider();
};

#endif // REOSDATAPROVIDERGUI_H
