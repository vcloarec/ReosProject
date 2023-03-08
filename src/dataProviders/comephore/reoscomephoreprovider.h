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
#include "reosnetcdfutils.h"

#define COMEPHORES_KEY QStringLiteral("comephore")

class ReosComephoreFilesReader
{
  public:
    virtual  ~ReosComephoreFilesReader() = default;

    virtual ReosComephoreFilesReader *clone() const = 0;
    virtual int frameCount() const = 0;
    virtual QDateTime time( int i ) const = 0;
    virtual QVector<int> data( int index, bool &readLine ) const = 0;
    virtual ReosRasterExtent extent() const = 0;
    virtual bool getDirectMinMax( double &min, double &max ) const = 0;
};

class ReosComephoreTiffFilesReader : public ReosComephoreFilesReader
{
  public:

    explicit ReosComephoreTiffFilesReader( const QString &folderPath );
    ~ReosComephoreTiffFilesReader();

    ReosComephoreFilesReader *clone() const override;

    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<int> data( int index, bool &readLine ) const override;
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;

    static bool canReadFile( const QString &uri );
    static ReosGriddedRainfallProvider::FileDetails details( const QString &source, bool *ok );

  private:
    ReosComephoreTiffFilesReader() = default;
    QMap<QDateTime, QString> mFilesNames;
    QList<QDateTime> mTimes;
};

class ReosComephoreNetCdfFilesReader : public ReosComephoreFilesReader
{
  public:
    explicit ReosComephoreNetCdfFilesReader( const QString &filePath );

    ReosComephoreFilesReader *clone() const override;
    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<int> data( int index, bool &readLine ) const override;
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;

    static bool canReadFile( const QString &uri );

  private:
    mutable std::unique_ptr<ReosNetCdfFile> mFile;
    QString mFileName;
    ReosRasterExtent mExtent;
    int mFrameCount = 0;
    QList<QDateTime> mTimes;
};

class ReosComephoreProvider : public ReosGriddedRainfallProvider
{
  public:
    ReosComephoreProvider();
    ~ReosComephoreProvider();
    ReosGriddedRainfallProvider *clone() const override;

    QString key() const override {return staticKey();}
    void setDataSource( const QString &dataSource ) override;
    QStringList fileSuffixes() const override;

    bool isValid() const override;
    int count() const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    const QVector<double> data( int index ) const override;
    ReosRasterExtent extent() const override;
    bool canReadUri( const QString &uri ) const override;
    FileDetails details( const QString &source, ReosModule::Message &message ) const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;
    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;

    static QString staticKey();
    static QString dataType();

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosComephoreFilesReader> mFileReader;
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
