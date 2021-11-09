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

class ReosMap;
class ReosDataObject;

class ReosDataProviderSelectorWidget : public QWidget
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

  signals:
    void dataSelectionChanged( bool dataIsSelected );
    void dataIsLoading();
    void dataIsReady();

  public slots:
    virtual void onClosed() {};
    virtual void onOpened() {};
};

class ReosDataProviderGuiFactory
{
    Q_GADGET
  public:
    enum class GuiCapability
    {
      DataSelector = 1 << 0, //!< If the driver can persist datasets defined on faces
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
    virtual ReosDataProviderSelectorWidget *createProviderSelectorWidget( ReosMap *map, QWidget *parent = nullptr ) const;

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
class ReosDataProviderGuiRegistery
{
  public:
    ReosDataProviderGuiRegistery();

    //! Registers a data provider gui factory
    void registerProviderGuiFactory( ReosDataProviderGuiFactory *factory );

    //! Returns all the data provider keys of type \a dataType that have a gui that have \a capability
    QStringList providers( const QString &dataType, ReosDataProviderGuiFactory::GuiCapability capability ) const;

    //! Returns the gui factory of the provider with \a key
    ReosDataProviderGuiFactory *guiFactory( const QString &key ) const;

    //! Returns the icon that represens the provider with the \a key
    QPixmap providerIcon( const QString &key ) const;

    //! Returns a text representing this data provider with \a key
    QString providerDisplayText( const QString &key ) const;

    //! Returns the static instance of this registery
    static ReosDataProviderGuiRegistery *instance();

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosConcentrationTimeFormula> dummy; // work arround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif

    std::map<QString, std::unique_ptr<ReosDataProviderGuiFactory>> mFactories;
    static ReosDataProviderGuiRegistery *sInstance;

    void loadDynamicProvider();
};

#endif // REOSDATAPROVIDERGUI_H
