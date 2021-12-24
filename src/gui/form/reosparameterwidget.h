/***************************************************************************
  reosparameterwidget.h - ReosParameterWidget

 ---------------------
 begin                : 22.1.2021
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
#ifndef REOSPARAMETERWIDGET_H
#define REOSPARAMETERWIDGET_H

#include <QPointer>
#include <QWidget>
#include <QComboBox>
#include <QTextEdit>

class QLabel;
class QLineEdit;
class QComboBox;
class QToolButton;
class QDateTimeEdit;
class QBoxLayout;
class QSpacerItem;
class QCheckBox;

#include "reosparameter.h"

class ReosParameterWidget : public QWidget
{
    Q_OBJECT
  public:
    enum SpacerPosition
    {
      NoSpacer = 1 << 0,
      SpacerBefore = 1 << 1, //!< Include a spacer before the label
      SpacerInMiddle = 1 << 2, //!< Include a spacer between label and value
      SpacerAfter = 1 << 3, //!< Include a spacer after the value
    };

    explicit ReosParameterWidget( const QString &defaultName, QWidget *parent = nullptr, Qt::Orientation orientation = Qt::Horizontal );

    //! Sets the keyboard focus on the edit part of the widget
    virtual void setFocusOnEdit() = 0;
    static ReosParameterWidget *createWidget( ReosParameter *parameter, QWidget *parent = nullptr );

    void enableSpacer( SpacerPosition spacerPosition );
    void setDefaultName( const QString &defaultName );
    void hideWhenVoid( bool b );
    void canBealwaysHidden();

  public slots:
    //! Updates the dispayed value from the parameter
    virtual void updateValue() = 0;
    //! Sets the parameter value with the displayed value
    virtual void applyValue() = 0;
    virtual void askDerivation();

  signals:
    void valueChanged();
    void unitChanged();

  protected:
    void finalizeWidget();
    void setParameter( ReosParameter *param );
    bool mHideWhenVoid = true;
    bool mCanBeAlwaysHidden = true;
    QPointer<ReosParameter> mParameter = nullptr;

  private:
    QLabel *mLabelName = nullptr;
    QToolButton *mDerivationButton = nullptr;
    QBoxLayout *mLayout = nullptr;
    QSpacerItem *mSpacerBefore = nullptr;
    QSpacerItem *mSpacerMiddle = nullptr;
    QSpacerItem *mSpacerAfter = nullptr;
    QString mDefaultName;

};

class ReosParameterInLineWidget : public ReosParameterWidget
{
  public:
    ReosParameterInLineWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    void setFocusOnEdit() override;

  protected:
    void setTextValue( double value );
    void setTextValue( const QString &str );
    double value() const;
    QString textValue() const;
    bool textHasChanged() const;
  private:
    QLineEdit *mLineEdit = nullptr;
    mutable QString mCurrentText;
};

class ReosParameterDoubleWidget : public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterDoubleWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDoubleWidget( ReosParameterDouble *value, QWidget *parent = nullptr );

    void setDouble( ReosParameterDouble *value );
    void updateValue();
    void applyValue();
    static QString type() {return QStringLiteral( "double" );}
    ReosParameterDouble *doubleParameter();
};

class ReosParameterIntegerWidget : public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterIntegerWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterIntegerWidget( ReosParameterInteger *value, QWidget *parent = nullptr );

    void setInteger( ReosParameterInteger *value );
    void updateValue();
    void applyValue();
    static QString type() {return QStringLiteral( "integer" );}
    ReosParameterInteger *integerParameter();
};

class ReosParameterStringWidget : public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterStringWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterStringWidget( ReosParameterString *string, QWidget *parent = nullptr );

    void setString( ReosParameterString *string );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "string" );}
    ReosParameterString *stringParameter();
};

class ReosParameterAreaWidget: public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterAreaWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterAreaWidget( ReosParameterArea *area, QWidget *parent = nullptr );

    void setArea( ReosParameterArea *area );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "area" );}

    ReosParameterArea *areaParameter() const;

  private:
    QComboBox *mUnitCombobox = nullptr;
};

class ReosParameterSlopeWidget: public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterSlopeWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterSlopeWidget( ReosParameterSlope *slope, QWidget *parent = nullptr );

    void setSlope( ReosParameterSlope *slope );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "slope" );}

    ReosParameterSlope *slopeParameter() const;

  private:

    QLabel *mLabelSlopeUnit = nullptr;
    int mFactor = 100;
};

class ReosDurationUnitComboBox: public QComboBox
{
  public:
    ReosDurationUnitComboBox( QWidget *parent = nullptr, ReosDuration::Unit timeUnit = ReosDuration::minute );

    //! Returns the current time unit
    ReosDuration::Unit currentUnit() const;

    //! Sets the current time unit
    void setCurrentUnit( ReosDuration::Unit unit );
};

class ReosParameterDurationWidget: public ReosParameterInLineWidget
{
  public:
    explicit ReosParameterDurationWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDurationWidget( ReosParameterDuration *duration, QWidget *parent = nullptr );

    void setDuration( ReosParameterDuration *duration );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "duration" );}

    ReosParameterDuration *durationParameter() const;

  private:

    ReosDurationUnitComboBox *mUnitCombobox = nullptr;
};

class ReosParameterDateTimeWidget: public ReosParameterWidget
{
  public:
    explicit ReosParameterDateTimeWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDateTimeWidget( ReosParameterDateTime *dateTime, QWidget *parent = nullptr );

    void setDateTime( ReosParameterDateTime *dateTime );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "date-time" );}

    ReosParameterDateTime *dateTimeParameter() const;

  private:
    QDateTimeEdit *mDateTimeEdit = nullptr;
};

class ReosParameterBooleanWidget : public ReosParameterWidget
{
  public:
    explicit ReosParameterBooleanWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterBooleanWidget( ReosParameterBoolean *booleanParameter, QWidget *parent = nullptr );

    void setBooleanParameter( ReosParameterBoolean *boolean );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "boolean" );}

    ReosParameterBoolean *booleanParameter() const;

  private:
    QCheckBox *mCheckBox = nullptr;
};

// Derived from QTextEdit to have a signal editingFinished when the focus go out
class ReosParameterTextEdit: public QTextEdit
{
    Q_OBJECT
  public:
    ReosParameterTextEdit( QWidget *parent );

  signals:
    void editingFinished();

  protected:
    void focusOutEvent( QFocusEvent *event ) override;
};

class ReosParameterLongStringWidget : public ReosParameterWidget
{
  public:
    explicit ReosParameterLongStringWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterLongStringWidget( ReosParameterLongString *longStringParameter, QWidget *parent = nullptr );

    void setString( ReosParameterLongString *string );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "long-string" );}

    ReosParameterLongString *stringParameter() const;

  private:
    ReosParameterTextEdit *mTextEdit;

};
#endif // REOSPARAMETERWIDGET_H
