/***************************************************************************
                      reosform.h
                     --------------------------------------
Date                 : 31-03-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                :   projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSFORM_H
#define REOSFORM_H

#include <QWidget>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QLabel>
#include <QDebug>
#include <QComboBox>
#include <QCheckBox>
#include <QAction>
#include <QToolButton>

#include "../Quantity/reosarea.h"
#include "../Quantity/reosduration.h"

class ReosForm;
class ReosFormParameterSimpleBool;

//*********************************************************************************************
//****************************  Base Parameter class     **************************************
//*********************************************************************************************

class ReosFormParameterEditor: public QWidget
{
    Q_OBJECT
public:
    ReosFormParameterEditor():QWidget(),
        toolButtonAskForCalculate(new QToolButton(this))
    {
        toolButtonAskForCalculate->setVisible(false);
        toolButtonAskForCalculate->setIcon(QPixmap("://toolbar/calculatrice.png"));
        connect(toolButtonAskForCalculate,&QAbstractButton::clicked,this,&ReosFormParameterEditor::askForCalculateClicked);
    }
    virtual ~ReosFormParameterEditor(){}



signals:
    void askForCalculate();

protected:
    QToolButton *toolButtonAskForCalculate;

    QString setToSystemLocale(QString str)
    {
        const QLocale &cLocale=QLocale::c();
        const QLocale &systemLocale=QLocale::system();
        str.replace(cLocale.decimalPoint(),systemLocale.decimalPoint());
        return str;
    }
    QString setToCLocale(QString str)
    {
        const QLocale &cLocale=QLocale::c();
        const QLocale &systemLocale=QLocale::system();
        str.replace(systemLocale.decimalPoint(),cLocale.decimalPoint());

        return str;
    }

private slots:
    void askForCalculateClicked()
    {
        emit askForCalculate();
    }

};


class ReosFormObject:public QObject
{
public:
    ReosFormObject(QObject *parent):QObject(parent)
    {

    }

    virtual QWidget *getWidget()=0;
};

class ReosFormParameter: public ReosFormObject
{
    Q_OBJECT
public:
    ReosFormParameter(ReosForm *parent);
    virtual ~ReosFormParameter() {}
    virtual QWidget *getWidget();


signals:
    void askForValue();
    void valueEdited();

protected slots:
    void valueAsked()
    {
        emit askForValue();
    }
    virtual ReosFormParameterEditor *makeEdit()=0;



};

//*********************************************************************************************
//********************************Simple parameter*********************************************
//*********************************************************************************************

class ReosFormParameterEditorSimple:public ReosFormParameterEditor
{
    Q_OBJECT
public:
    ReosFormParameterEditorSimple(QString nameField, QString value):ReosFormParameterEditor()
    {
        lay=new QHBoxLayout;
        setLayout(lay);
        lay->setMargin(0);
        valueEdit=new QLineEdit(setToSystemLocale(value),this);
        lay->addWidget(new QLabel(nameField));
        lay->addStretch();
        lay->addWidget(valueEdit);

        connect(valueEdit,&QLineEdit::editingFinished,this,&ReosFormParameterEditorSimple::valueHasBeenEdited);

    }


    ReosFormParameterEditorSimple(QString nameField, QString value, QString unit):ReosFormParameterEditorSimple(nameField,value)
    {
        layout()->addWidget(new QLabel(unit));
    }

    virtual ~ReosFormParameterEditorSimple() {}


    void setAlignment(Qt::Alignment a)
    {
        valueEdit->setAlignment(a);
    }

    void enableCalculateButton()
    {
        toolButtonAskForCalculate->setVisible(true);
        lay->addWidget(toolButtonAskForCalculate);
    }

public slots:
    void updateValue(QString val)
    {
        QObject::blockSignals(true);
        setValueEditText(val);
        QObject::blockSignals(false);
    }

    void enableEditor(bool b)
    {
        valueEdit->setEnabled(b);
    }

    void disableEditor(bool b)
    {
        valueEdit->setEnabled(!b);
    }

    void setEditable(bool b)
    {
        valueEdit->setReadOnly(!b);
    }

signals:
    void changed(QString value);

protected:
    void setValueEditText(QString str)
    {
        valueEdit->setText(setToSystemLocale(str));
    }

private slots:
    void valueHasBeenEdited()
    {
        emit changed(setToCLocale(valueEdit->text()));
    }

private:
    QHBoxLayout *lay;
    QLineEdit *valueEdit;

};

class ReosFormParameterSimple: public ReosFormParameter
{
    Q_OBJECT
public:
    ReosFormParameterSimple(QString name, ReosForm* parent=nullptr):ReosFormParameter(parent),
        name(name)
    {}
    virtual ~ReosFormParameterSimple() override {}

    QString getName() const;
    void setName(const QString &value);

    virtual bool isDefined() const =0;
    virtual void setUndefined() =0;

    void enableCalculateButton()
    {
        calculateButton=true;
    }

    void linkEditableWithBool(ReosFormParameterSimpleBool *boolParameter,bool editableIfTrue);
    void linkEnableWithBool(ReosFormParameterSimpleBool *boolParameter,bool enableIfTrue);

signals:
    void valueChanged(QString value);
    void enable(bool b);
    void editableEditor(bool b);


public slots:
    void enableEditor(bool b)
    {
        disable=!b;
        emit enable(b);
    }
    void disableEditor(bool b)
    {
        disable=b;
        emit enable(!b);
    }

    void setEditable(bool b)
    {
        editable=b;
        emit editableEditor(b);
    }

    void setUneditable(bool b)
    {
        editable=!b;
        emit editableEditor(editable);
    }

protected:
    bool calculateButton=false;
    bool disable=false;
    bool editable=true;

private:
    QString name;


    virtual ReosFormParameterEditorSimple* instantiateEditor()=0;

    virtual QString getStringValue()=0;

protected slots:
    virtual ReosFormParameterEditor *makeEdit() override
    {
        ReosFormParameterEditorSimple* editor=instantiateEditor();
        connect(this,&ReosFormParameterSimple::valueChanged,editor,&ReosFormParameterEditorSimple::updateValue);
        connect(editor,&ReosFormParameterEditorSimple::changed,this,&ReosFormParameterSimple::valueChangeByEditor);
        connect(this,&ReosFormParameterSimple::enable,editor,&ReosFormParameterEditorSimple::enableEditor);
        connect(this,&ReosFormParameterSimple::editableEditor,editor,&ReosFormParameterEditorSimple::setEditable);
        editor->enableEditor(!disable);
        editor->setEditable(editable);
        if (calculateButton)
            editor->enableCalculateButton();
        return editor;
    }

    virtual void valueChangeByEditor(QString v)=0;

};

class ReosFormParameterEditorSimpleWithVariableUnit: public ReosFormParameterEditorSimple
{
    Q_OBJECT
public:
    ReosFormParameterEditorSimpleWithVariableUnit(QString nameField, QString value, QStringList units,int unitPos):
        ReosFormParameterEditorSimple(nameField,value)
    {
        QComboBox *comboBoxUnit=new QComboBox;
        layout()->addWidget(comboBoxUnit);
        comboBoxUnit->addItems(units);
        comboBoxUnit->setCurrentIndex(unitPos);
        connect(comboBoxUnit,QOverload<int>::of(&QComboBox::currentIndexChanged),this,&ReosFormParameterEditorSimpleWithVariableUnit::unitChanged);
    }

    virtual ~ReosFormParameterEditorSimpleWithVariableUnit() override {}

signals:
    void changeUnit(int i);


private slots:

    void unitChanged(int i)
    {
        emit changeUnit(i);
    }

};


//********************************double simple parameter*********************************************


class ReosFormParameterSimpleDouble:public ReosFormParameterSimple
{
    Q_OBJECT
public:
    ReosFormParameterSimpleDouble(QString name, double value, ReosForm* parent,QString unit=""):ReosFormParameterSimple(name,parent),
        value(value),unit(unit)
    {}

    double getValue() const;
    void setValue(double value);

    QString getUnit() const;
    void setUnit(const QString &value);

    void setPrecision(int p)
    {
        precision=p;
    }

    virtual bool isDefined() const override
    {
        return !(fabs(noData-value)<(fabs(noData)>fabs(value) ? fabs(noData) : fabs(value))*std::numeric_limits<double>::epsilon());
    }

    virtual void setUndefined() override
    {
        value=noData;
    }

    double getNoData() const;
    void setNoData(double value);

private:
    double value;
    double noData=-99999;
    QString unit;

    // HdFormParameterSimple interface
private:
    ReosFormParameterEditorSimple *instantiateEditor() override
    {
        QString valueString=getStringValue();
        ReosFormParameterEditorSimple* editor;
        if (getUnit()=="")
            editor= new ReosFormParameterEditorSimple(getName(),valueString);
        else {
            editor= new ReosFormParameterEditorSimple(getName(),valueString,getUnit());
        }

        editor->setAlignment(Qt::AlignRight);
        return editor;
    }

    int precision=6;

    QString getStringValue() override
    {
        if (isDefined())
        {
            return QString::number(value);
        }
        else {
             return QString("-");
        }
    }

protected slots:
    void valueChangeByEditor(QString v) override
    {
        value=v.toDouble();
        emit valueEdited();
        emit valueChanged(getStringValue());
    }

};

//********************************String parameter*********************************************

class ReosFormParameterSimpleString:public ReosFormParameterSimple
{
    Q_OBJECT
public:
    ReosFormParameterSimpleString(QString name, QString value,ReosForm* parent):ReosFormParameterSimple(name,parent),
        value(value)
    {}
    virtual ~ReosFormParameterSimpleString() override {}


    QString getValue() const;
    void setValue(const QString &v);

    virtual bool isDefined() const override
    {
        return value!="";
    }

    virtual void setUndefined() override
    {
        value="";
    }

private:
    QString value;

    // HdFormParameterSimple interface
private:
    ReosFormParameterEditorSimple *instantiateEditor() override
    {
        return new ReosFormParameterEditorSimple(getName(),value);
    }

    QString getStringValue() override
    {
        return value;
    }

protected slots:
    void valueChangeByEditor(QString v) override
    {
        value=v;
        emit valueEdited();
        emit valueChanged(v);
    }
};

//********************************Variable unit parameters*********************************************

class ReosFormParameterSimpleUnitVariable:public ReosFormParameterSimple
{
    Q_OBJECT
public:
    ReosFormParameterSimpleUnitVariable(QString name,ReosForm* parent):ReosFormParameterSimple(name,parent)
    {

    }

    QString getStringUnit() const {
        return getUnits().at(getCurrentUnit());
    }


signals:
    void unitChange(int i);

protected slots:
    virtual ReosFormParameterEditor *makeEdit() override
    {
        ReosFormParameterEditorSimpleWithVariableUnit* editor=instantiateEditor();
        connect(this,&ReosFormParameterSimple::valueChanged,editor,&ReosFormParameterEditorSimpleWithVariableUnit::updateValue);
        connect(editor,&ReosFormParameterEditorSimple::changed,this,&ReosFormParameterSimpleUnitVariable::valueChangeByEditor);
        connect(this,&ReosFormParameterSimpleUnitVariable::enable,editor,&ReosFormParameterEditorSimpleWithVariableUnit::enableEditor);
        connect(this,&ReosFormParameterSimpleUnitVariable::editableEditor,editor,&ReosFormParameterEditorSimpleWithVariableUnit::setEditable);
        connect(editor,&ReosFormParameterEditorSimpleWithVariableUnit::changeUnit,this,&ReosFormParameterSimpleUnitVariable::unitChangeByEditor);
        editor->setEditable(editable);
        editor->enableEditor(!disable);
        if (calculateButton)
            editor->enableCalculateButton();
        return editor;
    }

    void valueChangeByEditor(QString v) override
    {
        setValueFromDouble(v.toDouble());
        emit valueEdited();
        emit valueChanged(getStringValue());
        emit unitChange(getCurrentUnit());

    }

    void unitChangeByEditor(int i)
    {
        setUnit(i);
        emit valueChanged(getStringValue());
        emit unitChange(getCurrentUnit());
    }

    QString getStringValue() override
    {
        if (isDefined())
        {
            return QString::number(getValueDouble(),'f',2);
        }
        else {
            return QString("-");
        }
    }

    virtual QStringList getUnits() const =0;


private:

    ReosFormParameterEditorSimpleWithVariableUnit *instantiateEditor() override
    {
        int posUnit=getCurrentUnit();
        QString string=getStringValue();
        ReosFormParameterEditorSimpleWithVariableUnit *editor=new ReosFormParameterEditorSimpleWithVariableUnit(getName(),string,getUnits(),posUnit);
        editor->setAlignment(Qt::AlignRight);
        return editor;
    }



    virtual double getValueDouble() const =0;
    virtual void setValueFromDouble(double d) =0;
    virtual int getCurrentUnit() const =0;
    virtual double getValueInUnit(int i) const =0;
    virtual void setUnit(int i)=0;
};

class ReosFormParameterSimpleArea:public ReosFormParameterSimpleUnitVariable
{
    Q_OBJECT
public:
    ReosFormParameterSimpleArea(QString name, ReosArea value,ReosForm* parent):ReosFormParameterSimpleUnitVariable(name,parent),
        value(value)
    {}

    ReosArea getValue() const;
    void setValue(const ReosArea &value);

    virtual bool isDefined() const override
    {
        return value.getValueInUnit()>0;
    }

    virtual void setUndefined() override
    {
        value=ReosArea(-1,value.unit());
    }

private:
    ReosArea value;


    // HdFormParameterSimpleUnitVariable interface
private:
    QStringList getUnits() const override
    {
        QStringList units;
        units<<(QString("m").append(QChar(0x00B2)));
        units<<tr("a");
        units<<tr("ha");
        units<<QString("km").append(QChar(0x00B2));

        return units;
    }

    double getValueDouble() const override
    {
        return value.getValueInUnit();
    }

    virtual void setValueFromDouble(double d) override
    {
        value=ReosArea(d,value.unit());
    }

    int getCurrentUnit() const override
    {
        return value.unit();
    }

    virtual double getValueInUnit(int i) const override
    {
        auto u=static_cast<ReosArea::Unit>(i);
        return value.getValueInUnit(u);
    }

    virtual void setUnit(int i) override
    {
        value.setUnit(static_cast<ReosArea::Unit>(i));
    }

};

class ReosFormParameterSimpleDuration:public ReosFormParameterSimpleUnitVariable
{
    Q_OBJECT
public:
    ReosFormParameterSimpleDuration(QString name, ReosDuration value,ReosForm* parent):ReosFormParameterSimpleUnitVariable(name,parent),
        value(value)
    {}

    ReosDuration getValue() const;
    void setValue(const ReosDuration &v);


    virtual bool isDefined() const override
    {
        return value.getValueUnit()>=0;
    }

    virtual void setUndefined() override
    {
        value=ReosDuration(-1,value.unit());
    }

    QStringList getUnits() const override
    {
        QStringList units;
        units<<tr("s");
        units<<tr("mn");
        units<<tr("h");
        units<<tr("j");
        units<<tr("semaines");
        units<<tr("mois");
        units<<tr("années");
        return units;
    }

private:
    ReosDuration value;


    // HdFormParameterSimpleUnitVariable interface


    double getValueDouble() const override
    {
        return value.getValueUnit();
    }

    virtual void setValueFromDouble(double d) override
    {
        value=ReosDuration(d,value.unit());
    }

    int getCurrentUnit() const override
    {
        return value.unit();
    }

    virtual double getValueInUnit(int i) const override
    {
        auto u=static_cast<ReosDuration::Unit>(i);
        return value.getValueUnit(u);
    }

    virtual void setUnit(int i) override
    {
        value.setUnit(static_cast<ReosDuration::Unit>(i));
    }


};
//*********************************************************************************************
//********************************Simple parameter*********************************************
//*********************************************************************************************

class ReosFormParameterEditorCheckBox:public ReosFormParameterEditor
{
    Q_OBJECT
public:
    ReosFormParameterEditorCheckBox(QString nameField, bool value):ReosFormParameterEditor()
    {
        QHBoxLayout *lay=new QHBoxLayout;
        setLayout(lay);
        lay->setMargin(0);
        checkBox=new QCheckBox(nameField,this);
        updateValue(value);
        lay->addWidget(checkBox);


        connect(checkBox,&QCheckBox::clicked,this,&ReosFormParameterEditorCheckBox::valueHasBeenEdited);
    }

public slots:
    void updateValue(bool val)
    {
        QObject::blockSignals(true);
        checkBox->setChecked(val);
        QObject::blockSignals(false);
    }


signals:
    void changed(bool value);

private:
    QCheckBox *checkBox;


private slots:
    void valueHasBeenEdited()
    {
        emit changed(checkBox->isChecked());
    }

};

class ReosFormParameterSimpleBool:public ReosFormParameter
{
    Q_OBJECT
public:
    ReosFormParameterSimpleBool(QString name, bool value, ReosForm* parent):ReosFormParameter(parent),
        name(name),value(value)
    {

    }

    bool getValue() const;
    void setValue(bool value);

signals:
    void valueChanged(bool b);

private:
    QString name;
    bool value;


protected slots:
    void valueChangeByEditor(bool v)
    {
        value=v;
        emit valueEdited();
        emit valueChanged(v);
    }

    // HdFormParameter interface
protected slots:
    ReosFormParameterEditor *makeEdit() override
    {
        ReosFormParameterEditorCheckBox* editor=new ReosFormParameterEditorCheckBox(name,value);
        connect(this,&ReosFormParameterSimpleBool::valueChanged,editor,&ReosFormParameterEditorCheckBox::updateValue);
        connect(editor,&ReosFormParameterEditorCheckBox::changed,this,&ReosFormParameterSimpleBool::valueChangeByEditor);
        return editor;
    }
};

//*********************************************************************************************
//******************************  Base Form class     *****************************************
//*********************************************************************************************

class ReosForm: public ReosFormObject
{
    Q_OBJECT
public:
    ReosForm(QObject *parent=nullptr):ReosFormObject(parent) {}
    virtual ~ReosForm() override {}
    const QList<ReosFormObject *> &getFormObjectList() const;

    void addForm(ReosForm* childForm);
    void addParamater(ReosFormParameter *param);
    void addSeparator();
    bool isSperatorPresent(int i) const;

public slots:
    virtual void parameterEdit();

signals:
    void parameterEdited();

private:
    QList<ReosFormObject*> attachedFormObjects;
//    QList<HdFormParameter* > parameterList;
    QList<int> separatorsPosition;



    // HdFormObject interface
public:
    QWidget *getWidget() override;
};

class ReosFormWidget : public QWidget
{
    Q_OBJECT
public:
    explicit ReosFormWidget(ReosForm* form=nullptr,QWidget *parent = nullptr):QWidget(parent)
    {
        QVBoxLayout* lay=new QVBoxLayout;
        setLayout(lay);
        lay->setSpacing(3);

        if (form)
        {
            const QList<ReosFormObject* > &list=form->getFormObjectList();
            for (auto param:list)
            {
                if (form->isSperatorPresent(list.indexOf(param)))
                {
                    lay->addLayout(addSeparator());
                }
                layout()->addWidget(param->getWidget());
            }

            if (form->isSperatorPresent(list.count()))
            {
                lay->addLayout(addSeparator());
            }
        }
        else {
            layout()->addWidget(new QLabel(tr("Formulaire vide")));
        }
        lay->addStretch();
    }

signals:

public slots:


private:
    QLayout* addSeparator()
    {
        if (layout())
        {
            QHBoxLayout* layoutLine = new QHBoxLayout;
            layoutLine->setSpacing(0);
            layoutLine->setMargin(4);
            layoutLine->addStretch();

            QVBoxLayout* vbox = new QVBoxLayout;
            QFrame *lineFrame=new QFrame;
            lineFrame->setMinimumSize(20,4);
            lineFrame->setMaximumSize(500,4);
            lineFrame->setSizePolicy(QSizePolicy::Expanding,QSizePolicy::Minimum);
            lineFrame->setFrameShape(QFrame::Box);
            lineFrame->setFrameStyle(QFrame::Panel | QFrame::Sunken);
            vbox->addWidget(lineFrame);
            layoutLine->addLayout(vbox);
            layoutLine->addStretch();

            return layoutLine;
        }

        return nullptr;
    }
};



//*********************************************************************************************



#endif // REOSFORM_H
