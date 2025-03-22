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
    virtual QVector<int> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const {return QVector<int>();}
    virtual bool supportGridExtent() const {return false;}
    virtual ReosRasterExtent extent() const = 0;
    virtual bool getDirectMinMax( double &min, double &max ) const = 0;
    virtual bool hasQualif() const = 0;
    virtual QVector<int> qualifData( int index, bool &readLine ) const = 0;
};

class ReosComephoreTiffFilesReader : public ReosComephoreFilesReader
{
  public:

    explicit ReosComephoreTiffFilesReader( const QString &uri );
    ~ReosComephoreTiffFilesReader();

    ReosComephoreFilesReader *clone() const override;

    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<int> data( int index, bool &readLine ) const override;
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    bool hasQualif() const override {return false;}
    QVector<int> qualifData( int, bool & ) const override {return QVector<int>();}

    static bool canReadFile( const QString &uri );
    static ReosGriddedDataProvider::FileDetails details( const QString &source, bool *ok );

  private:
    ReosComephoreTiffFilesReader() = default;
    QMap<QDateTime, QString> mFilesNames;
    QList<QDateTime> mTimes;
};


/**
 * Class that read data in a NetCDF COMEPHORE file.
 * Specification of this format: https://www7.obs-mip.fr/wp-content-aeris/uploads/sites/59/2020/02/radar_produit_comephore.pdf
 * In the file, NetCDF dimensions are (time,X,Y). When reading data, as the last dimension is varying fastest, data are read by column.
 */
class ReosComephoreNetCdfFilesReader : public ReosComephoreFilesReader
{
  public:
    explicit ReosComephoreNetCdfFilesReader( const QString &uri );

    ReosComephoreFilesReader *clone() const override;
    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<int> data( int index, bool &readLine ) const override;
    QVector<int> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const override;
    bool supportGridExtent() const override {return true;}
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    bool hasQualif() const override {return true;}
    QVector<int> qualifData( int index, bool &readLine ) const override;

    static bool canReadFile( const QString &uri );

    void reset();

  private:
    ReosComephoreNetCdfFilesReader() = default;
    mutable std::unique_ptr<ReosNetCdfFile> mFile;
    QString mFileName;
    ReosRasterExtent mExtent;
    QList<QDateTime> mTimes;
    QMap<int, int> mRainIndexToFileIndex;
};



class ReosComephoreNetCdfFolderReader : public ReosComephoreFilesReader
{
  public:
    explicit ReosComephoreNetCdfFolderReader( const QString &folderPath );

    ReosComephoreFilesReader *clone() const override;
    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<int> data( int index, bool &readLine ) const override;
    QVector<int> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const override;
    bool supportGridExtent() const override {return true;}
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    bool hasQualif() const override {return true;}
    QVector<int> qualifData( int index, bool &readLine ) const override;

    static bool canReadFile( const QString &uri );

  private:
    QString mFolderPath;
    std::vector<std::unique_ptr<ReosComephoreNetCdfFilesReader>> mFileReaders;
    mutable size_t mLastFileIndex = -1;
    ReosRasterExtent mExtent;
    struct InternalIndex
    {
      size_t fileIndex = -1;
      int internIndex = -1;
    };
    QMap<int, InternalIndex> mGlobalIndexToReaderIndex;

    ReosComephoreNetCdfFilesReader *fileReader( int index, int &interINdex ) const;
};

class ReosComephoreProvider : public ReosGriddedDataProvider
{
  public:
    ReosComephoreProvider();
    ~ReosComephoreProvider();
    ReosGriddedDataProvider *clone() const override;
    void load() override;

    QString key() const override {return staticKey();}
    QStringList fileSuffixes() const override;
    QString htmlDescription() const override;
    bool hasCapability( GridCapability capability )  const override;

    bool isValid() const override;
    int count() const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    const QVector<double> data( int index ) const override;
    const QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const override;
    const QVector<double> qualifData( int index ) const override;
    ReosRasterExtent extent() const override;
    bool canReadUri( const QString &uri ) const override;
    FileDetails details( const QString &source, ReosModule::Message &message ) const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;
    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;

    void exportToTiff( int index, const QString &fileName ) const override;

    static QString staticKey();
    static QString dataType();

    static QVariantMap decodeUri( const QString &uri, bool &ok );
    static QString pathFromUri( const QString &uri );
    static QDateTime startFromUri( const QString &uri );
    static QDateTime endFromUri( const QString &uri );
    static QString replacePathInUri( const QString &uri, const QString &newPth );

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosComephoreFilesReader> mFileReader;
    ReosRasterExtent mExtent;
    mutable QCache<int, QVector<double>> mCache;
    GridCapabilities mCapabilities = {SubGridExtract | QualificationValue};

    QDateTime mStartTime;
    QDateTime mEndTime;

    static QStringList QStringLiterral( const char * );
};


class ReosComephoresProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedDataProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
    bool supportType( const QString &dataType ) const override;
    QVariantMap uriParameters( const QString &dataType ) const override;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const override;
};

#endif // REOSCOMEPHORESPROVIDER_H
