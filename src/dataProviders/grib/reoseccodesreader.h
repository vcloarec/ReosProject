#ifndef REOSECCODESREADER_H
#define REOSECCODESREADER_H

#include <memory>

#include <QString>
#include <QStringList>
#include <QVariantMap>

#include "eccodes.h"
#include "reosmemoryraster.h"
#include "reosgdalutils.h"
#include "reosduration.h"


struct ReosEcCodesGridDescritpion
{
  QString wktCrs;
  int width;
  int height;
  double north;
  double south;
  double west;
  double east;
  bool yAscendant;

};

class ReosEcCodesReaderKeys
{
  public:
    ReosEcCodesReaderKeys() = default;
    ReosEcCodesReaderKeys( codes_handle *handle );

    bool hasKey( const QString &key ) const;

    QString stringValue( const QString &key ) const;
    long longValue( const QString &key ) const;
    double doubleValue( const QString &key ) const;

    QVariantMap map() const;
  private:
    QVariantMap mMap;
};


class ReosEcCodesMessage
{
  public:
    ReosEcCodesMessage( FILE *mFile, long posInFile );
    ~ReosEcCodesMessage();

  private:
    FILE *mFile = nullptr;
    long mPosInFIle = 0;
};

class ReosEcCodesDataset
{
  public:
    void addMessage( const QDateTime &startTime, FILE *file, long poInFile );

    int frameCount() const;

  private:
    std::vector<std::unique_ptr<ReosEcCodesMessage>> mMessages;
};




class ReosEcCodesIndex
{
  public:
    ReosEcCodesIndex( const QString &fileName, const QVariantMap &variableKeys );
    ~ReosEcCodesIndex() = default;

    codes_handle *nextHandle( bool &ok );

    codes_handle *handle( int handleIndex );

    bool isValid() const;

  private:
    QString mFileName;
    QVariantMap mKeys;
    std::shared_ptr<codes_index> mIndex;
    int mNextIndex = 0;

    void reset();
};


class ReosEcCodesReader: public ReosGriddedDataSource
{
  public:

    struct Variable
    {
      QString name;
      QString shortName;
    };

    enum StepType
    {
      Accum,
      Instant,
      Unknown
    };

    ReosEcCodesReader( const QString &gribFileName, const QVariantMap &variableKeys );
    ~ReosEcCodesReader();

    ReosRasterExtent extent( int frameIndex ) const override;
    ReosRasterMemory<double>  values( int index ) const override;
    bool isValid() const override;

    int frameCount() const override;

    ReosEcCodesReaderKeys keys( int frameIndex ) const;

    QDateTime dataTime( int index ) const;
    QDateTime validityTime( int index ) const;

    StepType stepType( int index ) const;
    QPair<int, int> stepRange( int index ) const ;
    ReosDuration stepDuration( int index ) const;

    static QList<Variable> variables( const QString &fileName );

  private:
    QString mFileName;
    QVariantMap mVariableKeys;
    bool mIsValid = false;
    int mFrameCount = 0;
    mutable ReosEcCodesIndex mIndex;

    codes_handle *findHandle( int index ) const;

    mutable ReosEcCodesReaderKeys mCacheKeys;
    mutable int mCacheKeysIndex = -1;
    mutable ReosRasterMemory<double> mCacheValues;
    mutable int mCacheValuesIndex = -1;

    const ReosEcCodesReaderKeys &getKeys( int index ) const;

};


#endif // REOSECCODESREADER_H
