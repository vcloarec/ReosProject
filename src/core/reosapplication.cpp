/***************************************************************************
                      reosapplication.cpp
                     --------------------------------------
Date                 : 20-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosapplication.h"

#include <QThread>
#include <QMessageBox>
#include "reosmapextent.h"

ReosApplication::ReosApplication( int &argc, char **argv, int flag ): QApplication( argc, argv, flag )
{
  qRegisterMetaType<ReosSpatialPosition>( "ReosSpatialPosition" );
}

bool ReosApplication::notify( QObject *receiver, QEvent *event )
{
  bool done = true;
  try
  {
    done = QApplication::notify( receiver, event );
  }
  catch ( std::exception &e )
  {
    if ( qApp->thread() == QThread::currentThread() )
      QMessageBox::critical( activeWindow(), tr( "Error:" ), e.what() );
  }
  catch ( ... )
  {
    if ( qApp->thread() == QThread::currentThread() )
      QMessageBox::critical( activeWindow(), tr( "Exception" ), tr( "Unknown error" ) );
  }

  return done;
}

QString ReosApplication::i18nPath()
{
  return QApplication::applicationDirPath() + QStringLiteral( "/../i18n" );
}


QString ReosApplication::styleSheet()
{
  QString myStyle = QStringLiteral( ".overview{"
                                    "  font: 1.82em;"
                                    "  font-weight: bold;"
                                    "}"
                                    "body{"
                                    "  background: white;"
                                    "  color: black;"
                                    "  font-family: 'Lato', 'Open Sans', 'Lucida Grande', 'Segoe UI', 'Arial', sans-serif;"
                                    "  width: 100%;"
                                    "}"
                                    "h1{  background-color: #F6F6F6;"
                                    "  color: #0996e6; "
                                    "  font-size: x-large;  "
                                    "  font-weight: normal;"
                                    "  background: none;"
                                    "  padding: 0.75em 0 0;"
                                    "  margin: 0;"
                                    "  line-height: 3em;"
                                    "}"
                                    "h2{  background-color: #F6F6F6;"
                                    "  color: #0978b8; "
                                    "  font-size: medium;  "
                                    "  font-weight: normal;"
                                    "  background: none;"
                                    "  padding: 0.75em 0 0;"
                                    "  margin: 0;"
                                    "  line-height: 1.1em;"
                                    "}"
                                    "h3{  background-color: #F6F6F6;"
                                    "  color: #515151;"
                                    "  font-weight: bold;"
                                    "  font-size: large;"
                                    "  text-align: left;"
                                    "  border-bottom: 5px solid #DCEB5C;"
                                    "}"
                                    "h4{  background-color: #F6F6F6;"
                                    "  color: #93b023;"
                                    "  font-weight: bold;"
                                    "  font-size: medium;"
                                    "  text-align: left;"
                                    "}"
                                    "h5{    background-color: #F6F6F6;"
                                    "   color: #93b023;"
                                    "   font-weight: bold;"
                                    "   font-size: small;"
                                    "   text-align: left;"
                                    "}"
                                    "a{  color: #729FCF;"
                                    "  font-family: arial,sans-serif;"
                                    "}"
                                    "label{  background-color: #FFFFCC;"
                                    "  border: 1px solid black;"
                                    "  margin: 1px;"
                                    "  padding: 0px 3px; "
                                    "  font-size: small;"
                                    "}"
                                    "th .strong {"
                                    "  font-weight: bold;"
                                    "}"
                                    "hr {"
                                    "  border: 0;"
                                    "  height: 0;"
                                    "  border-top: 1px solid black;"
                                    "}"
                                    ".list-view .highlight {"
                                    "  text-align: left;"
                                    "  border: 0px;"
                                    "  width: 20%;"
                                    "  padding-right: 15px;"
                                    "  padding-left: 20px;"
                                    "  font-weight: bold;"
                                    "}"
                                    ".tabular-view .odd-row {"
                                    "  background-color: #f9f9f9;"
                                    "}"
                                    ".section {"
                                    "  font-weight: bold;"
                                    "  padding-top:25px;"
                                    "}" );


  myStyle += QStringLiteral(
               ".tabular-view{ "
               "  border-collapse: collapse;"
               "  width: 95%;"
               "}"
               ".tabular-view th, .tabular-view td { "
               "  border:1px solid black;"
               "}" );

  myStyle.append( QStringLiteral( "body { margin: 10px; }\n " ) );

  return myStyle;
}
