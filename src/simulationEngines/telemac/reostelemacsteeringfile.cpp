/***************************************************************************
  reostelemacsteeringfile.h - ReosTelemacSteeringFile

 ---------------------
 begin                : 2.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reostelemacsteeringfile.h"
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>


static QString unquote( const QString &str )
{
  return str.trimmed().remove( QRegularExpression( "^'|'$" ) );
}

static int toInt( const QString &strValue )
{
  if ( strValue.isEmpty() )
    return 0;

  bool ok = false;
  int ret = strValue.toInt( &ok );
  if ( !ok )
    return 0;

  return ret;
}

static double toDouble( const QString &strValue )
{
  if ( strValue.isEmpty() )
    return 0;

  bool ok = false;
  double ret = strValue.toDouble( &ok );
  if ( !ok )
    return 0;

  return ret;
}


ReosTelemacSteeringFile::ReosTelemacSteeringFile( const QString &path )
  : mPath( path )
{}

//! function below is used to extract line that can be on several text line in the file.
static QPair<QString, QString> extractLine( QTextStream &stream, bool &endOfFile )
{
  QString key;
  QString var;
  bool inKey = false;
  bool inQuote = false;
  bool inVariable = false;
  bool varSeparator = false;

  while ( !stream.atEnd() )
  {
    QChar ch;

    do
    {
      stream >> ch;
    } while ( !stream.atEnd() && ( ch.isSpace() || ch == '\n' ) );

    if ( ch == '/' )
    {
      QString comment;
      while ( !stream.atEnd() )
      {
        stream >> ch;
        if ( ch == '\n' )
          break;
        comment += ch;
      }
      return QPair<QString, QString>( "COMMENT", comment );
    }

    inKey = true;
    key += ch;
    while ( inKey )
    {
      stream >> ch;
      if ( ch == '=' || ch == ':' )
      {
        inKey = false;
        break;
      }
      else if ( ch == '\'' )
      {
        inQuote = !inQuote;
      }

      if ( inKey )
        key += ch;
    }

    while ( !inKey && !stream.atEnd() )
    {
      stream >> ch;
      if ( ch == '\'' )
      {
        inQuote = !inQuote;
        varSeparator = false;
        var += ch;
      }
      else if ( ch == ';' )
      {
        varSeparator = true;
        var += ch;
      }
      else if ( ch == '\n' )
      {
        if ( !inQuote && !stream.atEnd() )
        {
          stream >> ch;
          if ( ch != '\'' && ch != ';' )
          {
            stream.seek( stream.pos() - 1 );
            return QPair<QString, QString>( key.trimmed(), var.trimmed() );
          }
          if ( ch == '\'' )
          {
            inQuote = !inQuote;
            varSeparator = false;
          }
          var += ch;
        }
      }
      else
      {
        var += ch;
      }
    }

    return QPair<QString, QString>( key.trimmed(), var.trimmed() );

    // if ( readLine.isEmpty() )
    //   continue;

    // if ( readLine.startsWith( '/' ) ) // comment line


    // inKey = !inQuote && inKey;

    // int charPos = 0;
    // while ( inKey && charPos < readLine.size() )
    // {
    //   const QChar currentChar = readLine.at( charPos );
    //   if ( currentChar == '=' )
    //   {
    //     inKey = false;
    //     charPos++;
    //     break;
    //   }
    //   else if ( currentChar == ':' )
    //   {
    //     inKey = false;
    //     charPos++;
    //     break;
    //   }
    //   else if ( currentChar == '\'' )
    //   {
    //     inQuote = !inQuote;
    //   }

    //   if ( inKey )
    //     key += currentChar;

    //   charPos++;
    // }

    // while ( !inKey && charPos < readLine.size() )
    // {
    //   const QChar &currentChar = readLine.at( charPos );
    //   if ( currentChar == '\'' )
    //   {
    //     inQuote = !inQuote;
    //     varSeparator = false;
    //   }
    //   else if ( currentChar == ';' )
    //   {
    //     varSeparator = true;
    //   }

    //   if ( ( charPos == readLine.size() - 1 ) && currentChar != ';' && !inQuote )
    //   {
    //     var += currentChar;
    //     break;
    //   }

    //   if ( !inKey )
    //     var += currentChar;
    //   charPos++;
    // }
  }

  endOfFile = true;
  return QPair<QString, QString>( QString(), QString() );
}

void ReosTelemacSteeringFile::open()
{
  QFile file( mPath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
    return;

  QTextStream in( &file );
  bool endOfFile = false;

  while ( !in.atEnd() )
  {
    QPair<QString, QString> out = extractLine( in, endOfFile );
    std::unique_ptr<SteeringLine> steeringLine = std::make_unique<SteeringLine>( out.first, out.second );
    if ( steeringLine->key() != QStringLiteral( "COMMENT" ) )
      mLinesMap.insert( steeringLine->key(), steeringLine.get() );
    mLines.push_back( std::move( steeringLine ) );
  }
}

void ReosTelemacSteeringFile::setKey( const QString &key, const QString &value )
{
  SteeringLine *line = mLinesMap.value( key, nullptr );
  if ( line )
  {
    line->setValue( value );
  }
  else
  {
    std::unique_ptr<SteeringLine> newLine = std::make_unique<SteeringLine>( key, value );
    mLinesMap.insert( key, newLine.get() );
    mLines.push_back( std::move( newLine ) );
  }
}

int ReosTelemacSteeringFile::keyCount() const
{
  return mLinesMap.count();
}

int ReosTelemacSteeringFile::lineCount() const
{
  return mLines.size();
}

bool ReosTelemacSteeringFile::isValid() const
{
  return mLinesMap.count() > 0;
}

QString ReosTelemacSteeringFile::geomFileName() const
{
  return unquote( value( QStringLiteral( "GEOMETRY FILE" ) ) );
}

QString ReosTelemacSteeringFile::resultFileName() const
{
  return unquote( value( QStringLiteral( "RESULTS FILE" ) ) );
}

QString ReosTelemacSteeringFile::boundaryFileName() const
{
  return unquote( value( QStringLiteral( "BOUNDARY CONDITIONS FILE" ) ) );
}

QString ReosTelemacSteeringFile::boundaryLiquidFileName() const
{
  return unquote( value( QStringLiteral( "LIQUID BOUNDARIES FILE" ) ) );
}

const QDateTime ReosTelemacSteeringFile::referenceTime() const
{
  return QDateTime();
}

const ReosDuration ReosTelemacSteeringFile::duration() const
{
  QString durationStr = value( QStringLiteral( "DURATION" ) );
  if ( durationStr.isEmpty() )
    return ReosDuration();

  int durationSeconds = durationStr.toInt();
  return ReosDuration( durationSeconds, ReosDuration::second );
}

const ReosDuration ReosTelemacSteeringFile::timeStep() const
{
  QString timeStepStr = value( QStringLiteral( "TIME STEP" ) );
  if ( timeStepStr.isEmpty() )
    return ReosDuration();

  int durationSeconds = timeStepStr.toInt();
  return ReosDuration( durationSeconds, ReosDuration::second );
}

const QList<double> ReosTelemacSteeringFile::prescribedFlowRate() const
{
  const QStringList flowRatesString = value( "PRESCRIBED FLOWRATES" ).split( ';' );
  QList<double> flowRates;
  for ( const QString &str : flowRatesString )
  {
    bool ok = false;
    double flowRate = str.toDouble( &ok );
    if ( ok )
      flowRates.append( flowRate );
    else
      flowRates.append( 0 );
  }

  return flowRates;
}

const QList<double> ReosTelemacSteeringFile::prescibedElevation() const
{
  const QStringList elevationsString = value( "PRESCRIBED FLOWRATES" ).split( ';' );
  QList<double> elevations;
  for ( const QString &str : elevationsString )
  {
    bool ok = false;
    double flowRate = str.toDouble( &ok );
    if ( ok )
      elevations.append( flowRate );
    else
      elevations.append( 0 );
  }

  return elevations;
}

ReosTelemacSteeringFile::SteeringLine::SteeringLine( const QString &line )
{
  //example : TREATMENT OF FLUXES AT THE BOUNDARIES = 2;2
  // We want to have key='TREATMENT OF FLUXES AT THE BOUNDARIES' and value='2;2'
  if ( line.isEmpty() )
    return;

  if ( line.startsWith( '/' ) ) // comment line
  {
    mKey = "COMMENT";
    mValue = line.mid( 1 ).trimmed();
  }
  else if ( line.contains( '=' ) )
  {
    mKey = line.section( '=', 0, 0 ).trimmed();
    mValue = line.section( '=', 1 ).trimmed();
  }
  else if ( line.contains( ':' ) )
  {
    mKey = line.section( ':', 0, 0 ).trimmed();
    mValue = line.section( ':', 1 ).trimmed();
  }
  else
  {
    mKey = line.trimmed();
    mValue = QString();
  }
}

void ReosTelemacSteeringFile::SteeringLine::setValue( const QString &value )
{
  mValue = value;
}

ReosTelemacSteeringFile::SteeringLine::SteeringLine( const QString &key, const QString &value )
  : mKey( key )
  , mValue( value )
{}

void ReosTelemacSteeringFile::save( const QString &path )
{
  if ( !path.isEmpty() )
    mPath = path;

  QFile file( mPath );
  if ( !file.open( QIODevice::WriteOnly | QIODevice::Text ) )
    return;
  for ( const std::unique_ptr<SteeringLine> &line : mLines )
  {
    if ( line->key() == QStringLiteral( "COMMENT" ) )
      file.write( QString( "/ %1\n" ).arg( line->value() ).toUtf8() );
    else
    {
      if ( line->value().isEmpty() )
        file.write( QString( "%1\n" ).arg( line->key() ).toUtf8() );
      else
        file.write( QString( "%1 = %2\n" ).arg( line->key(), line->value() ).toUtf8() );
    }
  }
}

const QString &ReosTelemacSteeringFile::path() const
{
  return mPath;
}

QString ReosTelemacSteeringFile::value( const QString &key ) const
{
  return mLinesMap.value( key, nullptr ) ? mLinesMap.value( key )->value() : QString();
}

void ReosTelemacSteeringFile::addComment( const QString &comment )
{
  mLines.push_back( std::make_unique<SteeringLine>( "COMMENT", comment ) );
}

QString ReosTelemacSteeringFile::SteeringLine::key() const
{
  return mKey;
}

QString ReosTelemacSteeringFile::SteeringLine::value() const
{
  return mValue;
}

ReosTelemac::Equation ReosTelemacSteeringFile::equation() const
{
  QString equationString = value( QStringLiteral( "EQUATION" ) );

  if ( equationString.toUpper() == "'SAINT-VENANT FV'" )
    return ReosTelemac::Equation::FiniteVolume;
  else if ( equationString.toUpper() == "'SAINT-VENANT FE'" )
    return ReosTelemac::Equation::FiniteElement;
  else
    return ReosTelemac::Equation::SteeringFileDefined;
}

int ReosTelemacSteeringFile::outputPeriodResult2D() const
{
  return toInt( value( QStringLiteral( "GRAPHIC PRINTOUT PERIOD" ) ) );
}

int ReosTelemacSteeringFile::outputPeriodResultHydrograph() const
{
  return toInt( value( QStringLiteral( "LISTING PRINTOUT PERIOD" ) ) );
}

ReosTelemac2DInitialCondition::Type ReosTelemacSteeringFile::initialConditionType() const
{
  QString initialConditionString = value( QStringLiteral( "INITIAL CONDITIONS" ) );

  if ( initialConditionString.toUpper() == "'CONSTANT ELEVATION'" )
    return ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity;
  else
    return ReosTelemac2DInitialCondition::Type::SteeringFileDefined;
}

double ReosTelemacSteeringFile::courantNumber() const
{
  return toDouble( value( QStringLiteral( "DESIRED COURANT NUMBER" ) ) );
}

bool ReosTelemacSteeringFile::hasKey( const QString &key ) const
{
  return mLinesMap.contains( key );
}

QString ReosTelemacSteeringFile::geomFilePath() const
{
  if ( mPath.isEmpty() )
    return QString();

  return QFileInfo( mPath ).absoluteDir().absoluteFilePath( geomFileName() );
}
