/***************************************************************************
  reosdataprovider.h - ReosDataProvider

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
#ifndef REOSDATAPROVIDER_H
#define REOSDATAPROVIDER_H



#include <QObject>
#include <map>
#include <memory>

#include "reoscore.h"

#ifndef SIP_RUN

class REOSCORE_EXPORT ReosDataProvider : public QObject
{
    Q_OBJECT
  public:

    enum Capability
    {
      CanWrite = 1 << 0, //!< If the dataprovider support writting data
      Spatial = 1 << 1, //!< If data is related to a spatial position
      File = 1 << 2, //!< source of data is a file or a group of files
      Memory = 1 << 3, //! Data are stored in memory and in the project files
      Net = 1 << 4, //! Source of the data is on internet
    };

    Q_ENUM( Capability )
    Q_DECLARE_FLAGS( Capabilities, Capability )
    Q_FLAG( Capabilities )

    //! Returns the provider key
    virtual QString key() const = 0;

    //! Returns a html description of the data
    virtual QString htmlDescription() const {return QString();}

    //! Returns whether the provider can read the \a uri
    virtual bool canReadUri( const QString &uri ) const;

    virtual QStringList fileSuffixes() const = 0;

    virtual bool isLoading() const  {return false;}

  signals:
    void dataChanged();
    void dataReset();
    void loadingFinished();
};

class REOSCORE_EXPORT ReosDataProviderFactory
{
  public:
    //! Creates and returns a pointer to a provider, the caller has to take ownership
    virtual ReosDataProvider *createProvider( const QString &dataType = QString() ) const = 0;

    //! Creates a new data source, \a uri has to be conform to the related provider
    virtual bool createNewDataSource( const QString &uri, const QString &dataType, QString &error ) {return false;};

    //! Returns the provider key corresponding to this factory
    virtual QString key() const = 0;

    //! Returns whether the data provider has the \a capabilities with data type \a dataType
    virtual bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const;

    //! Returns whether the fctory support data type \a dataType
    virtual bool supportType( const QString &dataType ) const = 0;

    //! Returns the list of parameters necessary to build an uri for the data type \a dataType
    virtual QVariantMap uriParameters( const QString &dataType ) const = 0;

    //! Builds the uri coresponding to the  data type \a dataType with \a parameters
    virtual QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const = 0;

};

#endif // No SIP_RUN

/**
 * Class that stores time serie provider factory
 */
class REOSCORE_EXPORT ReosDataProviderRegistery
{
  public:
    ReosDataProviderRegistery();

    //! Registers a \a factory
    void registerProviderFactory( ReosDataProviderFactory *factory ) SIP_SKIP;

    //! Creates and returns a provider corresponding to the \a key. Caller takes ownership
    ReosDataProvider *createProvider( const QString &key ) SIP_SKIP;

    //! Creates and returns a provider corresponding to the \a key ans \a dataType. Caller takes ownership
    ReosDataProvider *createProvider( const QString &key, const QString &dataType ) SIP_SKIP;

    //! Creates a provider that can read the data pointed by \a uri
    ReosDataProvider *createCompatibleProvider( const QString &uri, const QString &dataType ) const SIP_SKIP;

    //! Returns a list of providers that support the type of data \a dataType
    QStringList providers( const QString &dataType ) const;

    //! Returns a list of data providers that have the \a capabilities with data type \a dataType
    QStringList withCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const SIP_SKIP;

    //! Returns a list of parameters that define the uri of the provider with \a key and with type \a dataType
    QVariantMap uriParameters( const QString &key, const QString &dataType );

    /**
     * Build the uri for the provider with \a key for data type \a dataType and with \a parameters.
     * Returns false if parameters are not valid
     */
    QString buildUri( const QString &key, const QString &dataType, const QVariantMap &parameters, bool &ok SIP_OUT );

    //! Returns a pointer to the static instance of this registery
    static ReosDataProviderRegistery *instance();

  private:
    std::map<QString, std::shared_ptr<ReosDataProviderFactory>> mFactories;

    // List of providers used to retrieve the compatible providers following an order
    // For now, providers are ordered following the order of the library files in the folder
    // TODO: implement an order for providers (1: priority, 2: lower priority,..).
    QList<ReosDataProviderFactory *> mListedFactories;
    static ReosDataProviderRegistery *sInstance;
    void loadDynamicProvider();

    ReosDataProviderFactory *extractFactory( const QString &key, QString &dataType ) const;
};


#endif // REOSDATAPROVIDER_H
