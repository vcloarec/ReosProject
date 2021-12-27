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

class ReosDataProvider : public QObject
{
    Q_OBJECT
  public:

    //! Returns the provider key
    virtual QString key() const = 0;

    //! Returns a html description of the data
    virtual QString htmlDescription() const {return QString();}

  signals:
    void dataChanged();
    void dataReset();
};

class ReosDataProviderFactory
{
  public:
    //! Creates and returns a pointer to a provider, the caller has to take ownership
    virtual ReosDataProvider *createProvider( const QString &dataType = QString() ) const = 0;

    //! Returns the provider key corresponding to this factory
    virtual QString key() const = 0;
};

/**
 * Class that stores time serie provider factory
 */
class ReosDataProviderRegistery
{
  public:
    ReosDataProviderRegistery();

    //! Registers a \a factory
    void registerProviderFactory( ReosDataProviderFactory *factory );

    //! Creates and returns a provider corresponding to the \a key
    ReosDataProvider *createProvider( const QString &key );

    //! Returns a pointer to the static instance of this registery
    static ReosDataProviderRegistery *instance();

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosConcentrationTimeFormula> dummy; // work arround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif

    std::map<QString, std::unique_ptr<ReosDataProviderFactory>> mFactories;
    static ReosDataProviderRegistery *sInstance;
    void loadDynamicProvider();
};


#endif // REOSDATAPROVIDER_H
