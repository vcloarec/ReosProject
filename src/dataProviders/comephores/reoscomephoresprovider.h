/***************************************************************************
  reoscomephoresprovider.h - ReosComephoresProvider

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
#ifndef REOSCOMEPHORESPROVIDER_H
#define REOSCOMEPHORESPROVIDER_H

#include <QCache>

#include "reosgriddedrainfallprovider.h"

#define COMEPHORES_KEY QStringLiteral("comephores")

class ReosComephoresFilesReader
{
  public:
    virtual  ~ReosComephoresFilesReader() = default;

    virtual ReosComephoresFilesReader *clone() const = 0;
    virtual int frameCount() const = 0;
    virtual QDateTime time( int i ) const = 0;
    virtual QVector<double> data( int index ) const = 0;
    virtual ReosRasterExtent extent() const = 0;
};

class ReosComephoresTiffFilesReader : public ReosComephoresFilesReader
{
  public:

    explicit ReosComephoresTiffFilesReader( const QString &folderPath );
    ~ReosComephoresTiffFilesReader();

    ReosComephoresFilesReader *clone() const override;

    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<double> data( int index ) const override;

    ReosRasterExtent extent() const override;

    static bool canReadFile( const QString &uri );
    static ReosGriddedRainfallProvider::Details details( const QString &source, bool *ok );

  private:
    ReosComephoresTiffFilesReader() = default;
    QMap<QDateTime, QString> mFilesNames;
    QList<QDateTime> mTimes;
};

class ReosComephoresProvider : public ReosGriddedRainfallProvider
{
  public:
    ReosComephoresProvider();
    ~ReosComephoresProvider();
    ReosGriddedRainfallProvider *clone() const override;

    QString key() const override {return staticKey();}
    void setDataSource( const QString &dataSource ) override;

    bool isValid() const override;
    int count() const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    const QVector<double> data( int index ) const override;
    ReosRasterExtent extent() const override;
    bool canReadUri( const QString &uri ) const override;
    Details details( const QString &source, ReosModule::Message &message ) const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context );

    static QString staticKey();
    static QString dataType();

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosComephoresFilesReader> mFileReader;
    ReosRasterExtent mExtent;
    mutable QCache<int, QVector<double>> mCache;
};

class ReosComephoresProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedRainfallProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
};

#endif // REOSCOMEPHORESPROVIDER_H
