/***************************************************************************
                      reosform.cpp
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

#include "reosform.h"


const QList<ReosFormObject *> &ReosForm::getFormObjectList() const
{
    return attachedFormObjects;
}

void ReosForm::addForm(ReosForm *childForm)
{
    attachedFormObjects.append(childForm);
}

void ReosForm::addParamater(ReosFormParameter *param)
{
    connect(param,&ReosFormParameter::valueEdited,this,&ReosForm::parameterEdit);
    attachedFormObjects.append(param);
}

void ReosForm::addForm(ReosFormObject *formObject)
{
    attachedFormObjects.append(formObject);
}

void ReosForm::addSeparator()
{
    separatorsPosition.append(attachedFormObjects.count());
}

bool ReosForm::isSeparatorPresent(int i) const
{
    return  separatorsPosition.contains(i);
}

void ReosForm::setOrientation(Qt::Orientation orientation)
{
    mOrientation=orientation;
}

Qt::Orientation ReosForm::orientation() const {return mOrientation;}

void ReosForm::parameterEdit()
{
    emit parameterEdited();
}

QWidget *ReosForm::getWidget()
{
    return new ReosFormWidget(this,nullptr,mOrientation);
}

ReosFormParameter::ReosFormParameter(ReosForm *parent):ReosFormObject (parent)
{
    if (parent)
        parent->addParamater(this);
}

QWidget *ReosFormParameter::getWidget()
{
    ReosFormParameterEditor *editor=makeEdit();
    connect(editor,&ReosFormParameterEditor::askForCalculate,this,&ReosFormParameter::valueAsked);

    return editor;
}

void ReosFormParameter::valueAsked()
{
    emit askForValue();
}


ReosFormParameterSimple::ReosFormParameterSimple(QString name, ReosForm *parent):ReosFormParameter(parent),
    name(name)
{}

QString ReosFormParameterSimple::getName() const
{
    return name;
}

void ReosFormParameterSimple::setName(const QString &value)
{
    name = value;
}

void ReosFormParameterSimple::enableCalculateButton()
{
    calculateButton=true;
}

void ReosFormParameterSimple::linkEditableWithBool(ReosFormParameterSimpleBool *boolParameter, bool editableIfTrue)
{
    if (editableIfTrue)
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::setEditable);
    else {
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::setUneditable);
    }
}

void ReosFormParameterSimple::linkEnableWithBool(ReosFormParameterSimpleBool *boolParameter, bool enableIfTrue)
{
    if (enableIfTrue)
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::enableEditor);
    else {
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::disableEditor);
    }
}

void ReosFormParameterSimple::enableEditor(bool b)
{
    disable=!b;
    emit enable(b);
}

void ReosFormParameterSimple::disableEditor(bool b)
{
    disable=b;
    emit enable(!b);
}

void ReosFormParameterSimple::setEditable(bool b)
{
    editable=b;
    emit editableEditor(b);
}

void ReosFormParameterSimple::setUneditable(bool b)
{
    editable=!b;
    emit editableEditor(editable);
}

ReosFormParameterEditor *ReosFormParameterSimple::makeEdit()
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


ReosFormParameterSimpleDouble::ReosFormParameterSimpleDouble(QString name, double value, ReosForm *parent, QString unit):ReosFormParameterSimple(name,parent),
    value(value),unit(unit)
{}

double ReosFormParameterSimpleDouble::getValue() const
{
    return value;
}

void ReosFormParameterSimpleDouble::setValue(double v)
{
    value = v;
    emit valueChanged(getStringValue());

}

QString ReosFormParameterSimpleDouble::getUnit() const
{
    return unit;
}

void ReosFormParameterSimpleDouble::setUnit(const QString &value)
{
    unit = value;
}

void ReosFormParameterSimpleDouble::setPrecision(int p)
{
    precision=p;
}

bool ReosFormParameterSimpleDouble::isDefined() const
{
    return !(fabs(noData-value)<(fabs(noData)>fabs(value) ? fabs(noData) : fabs(value))*std::numeric_limits<double>::epsilon());
}

void ReosFormParameterSimpleDouble::setUndefined()
{
    value=noData;
}

double ReosFormParameterSimpleDouble::getNoData() const
{
    return noData;
}

void ReosFormParameterSimpleDouble::setNoData(double value)
{
    noData = value;
}

ReosFormParameterEditorSimple *ReosFormParameterSimpleDouble::instantiateEditor()
{
    QString valueString=getStringValue();
    ReosFormParameterEditorSimple* editor;
    if (getUnit()=="")
        editor= new ReosFormParameterEditorSimple(getName(),valueString);
    else {
        editor= new ReosFormParameterEditorSimple(getName(),valueString,getUnit());
    }

    editor->setAlignment(alignment);
    return editor;
}

QString ReosFormParameterSimpleDouble::getStringValue()
{
    if (isDefined())
    {
        return QString::number(value);
    }
    else {
        return QString("-");
    }
}

void ReosFormParameterSimpleDouble::valueChangeByEditor(QString v)
{
    value=v.toDouble();
    emit valueEdited();
    emit valueChanged(getStringValue());
}

ReosFormParameterSimpleString::ReosFormParameterSimpleString(QString name, QString value, ReosForm *parent):ReosFormParameterSimple(name,parent),
    value(value)
{}

QString ReosFormParameterSimpleString::getValue() const
{
    return value;
}

void ReosFormParameterSimpleString::setValue(const QString &v)
{
    value = v;
    emit valueChanged(value);
}

bool ReosFormParameterSimpleString::isDefined() const
{
    return value!="";
}

void ReosFormParameterSimpleString::setUndefined()
{
    value="";
}

ReosFormParameterEditorSimple *ReosFormParameterSimpleString::instantiateEditor()
{
    return new ReosFormParameterEditorSimple(getName(),value);
}

QString ReosFormParameterSimpleString::getStringValue()
{
    return value;
}

void ReosFormParameterSimpleString::valueChangeByEditor(QString v)
{
    value=v;
    emit valueEdited();
    emit valueChanged(v);
}

ReosFormParameterSimpleBool::ReosFormParameterSimpleBool(QString name, bool value, ReosForm *parent):ReosFormParameter(parent),
    name(name),value(value)
{

}

bool ReosFormParameterSimpleBool::getValue() const
{
    return value;
}

void ReosFormParameterSimpleBool::setValue(bool v)
{
    value = v;
    emit valueChanged(value);
}

void ReosFormParameterSimpleBool::valueChangeByEditor(bool v)
{
    value=v;
    emit valueEdited();
    emit valueChanged(v);
}

ReosFormParameterEditor *ReosFormParameterSimpleBool::makeEdit()
{
    ReosFormParameterEditorCheckBox* editor=new ReosFormParameterEditorCheckBox(name,value);
    connect(this,&ReosFormParameterSimpleBool::valueChanged,editor,&ReosFormParameterEditorCheckBox::updateValue);
    connect(editor,&ReosFormParameterEditorCheckBox::changed,this,&ReosFormParameterSimpleBool::valueChangeByEditor);
    return editor;
}

ReosArea ReosFormParameterSimpleArea::getValue() const
{
    return value;
}

void ReosFormParameterSimpleArea::setValue(const ReosArea &v)
{
    value = v;
    emit valueChanged(getStringValue());
    emit unitChange(value.unit());

}

bool ReosFormParameterSimpleArea::isDefined() const
{
    return value.getValueInUnit()>0;
}

void ReosFormParameterSimpleArea::setUndefined()
{
    value=ReosArea(-1,value.unit());
}

QStringList ReosFormParameterSimpleArea::getUnits() const
{
    QStringList units;
    units<<(QString("m").append(QChar(0x00B2)));
    units<<tr("a");
    units<<tr("ha");
    units<<QString("km").append(QChar(0x00B2));

    return units;
}

double ReosFormParameterSimpleArea::getValueDouble() const
{
    return value.getValueInUnit();
}

void ReosFormParameterSimpleArea::setValueFromDouble(double d)
{
    value=ReosArea(d,value.unit());
}

int ReosFormParameterSimpleArea::getCurrentUnit() const
{
    return value.unit();
}

double ReosFormParameterSimpleArea::getValueInUnit(int i) const
{
    auto u=static_cast<ReosArea::Unit>(i);
    return value.getValueInUnit(u);
}

void ReosFormParameterSimpleArea::setUnit(int i)
{
    value.setUnit(static_cast<ReosArea::Unit>(i));
}

ReosFormParameterSimpleDuration::ReosFormParameterSimpleDuration(QString name, ReosDuration value, ReosForm *parent):ReosFormParameterSimpleUnitVariable(name,parent),
    value(value)
{}

ReosDuration ReosFormParameterSimpleDuration::getValue() const
{
    return value;
}

void ReosFormParameterSimpleDuration::setValue(const ReosDuration &v)
{
    value=v;
    emit valueChanged(getStringValue());
}

bool ReosFormParameterSimpleDuration::isDefined() const
{
    return value.getValueUnit()>=0;
}

void ReosFormParameterSimpleDuration::setUndefined()
{
    value=ReosDuration(-1,value.unit());
}

QStringList ReosFormParameterSimpleDuration::getUnits() const
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

double ReosFormParameterSimpleDuration::getValueDouble() const
{
    return value.getValueUnit();
}

void ReosFormParameterSimpleDuration::setValueFromDouble(double d)
{
    value=ReosDuration(d,value.unit());
}

int ReosFormParameterSimpleDuration::getCurrentUnit() const
{
    return value.unit();
}

double ReosFormParameterSimpleDuration::getValueInUnit(int i) const
{
    auto u=static_cast<ReosDuration::Unit>(i);
    return value.getValueUnit(u);
}

void ReosFormParameterSimpleDuration::setUnit(int i)
{
    value.setUnit(static_cast<ReosDuration::Unit>(i));
}

ReosFormParameterEditor::ReosFormParameterEditor():QWidget(),
    toolButtonAskForCalculate(new QToolButton(this))
{
    toolButtonAskForCalculate->setVisible(false);
    toolButtonAskForCalculate->setIcon(QPixmap("://toolbar/calculatrice.png"));
    connect(toolButtonAskForCalculate,&QAbstractButton::clicked,this,&ReosFormParameterEditor::askForCalculateClicked);
}

QString ReosFormParameterEditor::setToSystemLocale(QString str)
{
    const QLocale &cLocale=QLocale::c();
    const QLocale &systemLocale=QLocale::system();
    str.replace(cLocale.decimalPoint(),systemLocale.decimalPoint());
    return str;
}

QString ReosFormParameterEditor::setToCLocale(QString str)
{
    const QLocale &cLocale=QLocale::c();
    const QLocale &systemLocale=QLocale::system();
    str.replace(systemLocale.decimalPoint(),cLocale.decimalPoint());

    return str;
}

void ReosFormParameterEditor::askForCalculateClicked()
{
    emit askForCalculate();
}

ReosFormObject::ReosFormObject(QObject *parent):QObject(parent)
{

}

ReosFormParameterEditorSimple::ReosFormParameterEditorSimple(QString nameField, QString value):ReosFormParameterEditor()
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

ReosFormParameterEditorSimple::ReosFormParameterEditorSimple(QString nameField, QString value, QString unit):ReosFormParameterEditorSimple(nameField,value)
{
    layout()->addWidget(new QLabel(unit));
}

ReosFormParameterEditorSimple::~ReosFormParameterEditorSimple() {}

void ReosFormParameterEditorSimple::setAlignment(Qt::Alignment a)
{
    valueEdit->setAlignment(a);
}

void ReosFormParameterEditorSimple::enableCalculateButton()
{
    toolButtonAskForCalculate->setVisible(true);
    lay->addWidget(toolButtonAskForCalculate);
}

void ReosFormParameterEditorSimple::updateValue(QString val)
{
    QObject::blockSignals(true);
    setValueEditText(val);
    QObject::blockSignals(false);
}

void ReosFormParameterEditorSimple::enableEditor(bool b)
{
    valueEdit->setEnabled(b);
}

void ReosFormParameterEditorSimple::disableEditor(bool b)
{
    valueEdit->setEnabled(!b);
}

void ReosFormParameterEditorSimple::setEditable(bool b)
{
    valueEdit->setReadOnly(!b);
}

void ReosFormParameterEditorSimple::setValueEditText(QString str)
{
    valueEdit->setText(setToSystemLocale(str));
}

void ReosFormParameterEditorSimple::valueHasBeenEdited()
{
    emit changed(setToCLocale(valueEdit->text()));
}

ReosFormParameterEditorSimpleWithVariableUnit::ReosFormParameterEditorSimpleWithVariableUnit(QString nameField, QString value, QStringList units, int unitPos):
    ReosFormParameterEditorSimple(nameField,value)
{
    QComboBox *comboBoxUnit=new QComboBox;
    layout()->addWidget(comboBoxUnit);
    comboBoxUnit->addItems(units);
    comboBoxUnit->setCurrentIndex(unitPos);
    connect(comboBoxUnit,QOverload<int>::of(&QComboBox::currentIndexChanged),this,&ReosFormParameterEditorSimpleWithVariableUnit::unitChanged);
}

ReosFormParameterEditorSimpleWithVariableUnit::~ReosFormParameterEditorSimpleWithVariableUnit() {}

void ReosFormParameterEditorSimpleWithVariableUnit::unitChanged(int i)
{
    emit changeUnit(i);
}

ReosFormParameterSimpleUnitVariable::ReosFormParameterSimpleUnitVariable(QString name, ReosForm *parent):ReosFormParameterSimple(name,parent)
{

}

QString ReosFormParameterSimpleUnitVariable::getStringUnit() const {
    return getUnits().at(getCurrentUnit());
}

ReosFormParameterEditor *ReosFormParameterSimpleUnitVariable::makeEdit()
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

void ReosFormParameterSimpleUnitVariable::valueChangeByEditor(QString v)
{
    setValueFromDouble(v.toDouble());
    emit valueEdited();
    emit valueChanged(getStringValue());
    emit unitChange(getCurrentUnit());

}

void ReosFormParameterSimpleUnitVariable::unitChangeByEditor(int i)
{
    setUnit(i);
    emit valueChanged(getStringValue());
    emit unitChange(getCurrentUnit());
}

QString ReosFormParameterSimpleUnitVariable::getStringValue()
{
    if (isDefined())
    {
        return QString::number(getValueDouble(),'f',2);
    }
    else {
        return QString("-");
    }
}

ReosFormParameterEditorSimpleWithVariableUnit *ReosFormParameterSimpleUnitVariable::instantiateEditor()
{
    int posUnit=getCurrentUnit();
    QString string=getStringValue();
    ReosFormParameterEditorSimpleWithVariableUnit *editor=new ReosFormParameterEditorSimpleWithVariableUnit(getName(),string,getUnits(),posUnit);
    editor->setAlignment(Qt::AlignRight);
    return editor;
}

ReosFormParameterEditorCheckBox::ReosFormParameterEditorCheckBox(QString nameField, bool value):ReosFormParameterEditor()
{
    QHBoxLayout *lay=new QHBoxLayout;
    setLayout(lay);
    lay->setMargin(0);
    checkBox=new QCheckBox(nameField,this);
    updateValue(value);
    lay->addWidget(checkBox);


    connect(checkBox,&QCheckBox::clicked,this,&ReosFormParameterEditorCheckBox::valueHasBeenEdited);
}

void ReosFormParameterEditorCheckBox::updateValue(bool val)
{
    QObject::blockSignals(true);
    checkBox->setChecked(val);
    QObject::blockSignals(false);
}

void ReosFormParameterEditorCheckBox::valueHasBeenEdited()
{
    emit changed(checkBox->isChecked());
}

ReosFormWidget::ReosFormWidget(ReosForm *form, QWidget *parent, Qt::Orientation orientation):QWidget(parent)
{
    QBoxLayout* lay;
    if (orientation==Qt::Vertical)
        lay=new QVBoxLayout;
    else
        lay=new QHBoxLayout;

    setLayout(lay);
    lay->setSpacing(3);

    auto formOrientation=form->orientation();
    Qt::Orientation separatorOrientation;

    if (formOrientation==Qt::Vertical)
        separatorOrientation=Qt::Horizontal;
    else
        separatorOrientation=Qt::Vertical;


    if (form)
    {
        const QList<ReosFormObject* > &list=form->getFormObjectList();
        for (auto param:list)
        {
            if (form->isSeparatorPresent(list.indexOf(param)))
            {
                lay->addLayout(addSeparator(separatorOrientation));
            }
            layout()->addWidget(param->getWidget());
        }

        if (form->isSeparatorPresent(list.count()))
        {
            lay->addLayout(addSeparator(separatorOrientation));
        }
    }
    else {
        layout()->addWidget(new QLabel(tr("Formulaire vide")));
    }
    lay->addStretch();
}

QLayout *ReosFormWidget::addSeparator(Qt::Orientation orientation)
{
    if (layout())
    {
        if (orientation==Qt::Horizontal)
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
        else
        {
            QVBoxLayout* layoutLine = new QVBoxLayout;
            layoutLine->setSpacing(0);
            layoutLine->setMargin(4);
            layoutLine->addStretch();

            QHBoxLayout* hbox = new QHBoxLayout;
            QFrame *lineFrame=new QFrame;
            lineFrame->setMinimumSize(4,20);
            lineFrame->setMaximumSize(4,500);
            lineFrame->setSizePolicy(QSizePolicy::Expanding,QSizePolicy::Minimum);
            lineFrame->setFrameShape(QFrame::Box);
            lineFrame->setFrameStyle(QFrame::Panel | QFrame::Sunken);
            hbox->addWidget(lineFrame);
            layoutLine->addLayout(hbox);
            layoutLine->addStretch();

            return layoutLine;
        }

    }

    return nullptr;
}

ReosFormAction::ReosFormAction(ReosForm *parent, const QIcon &icon,const QString &text): ReosFormObject(parent)
{
    if (parent)
        parent->addForm(this);
    mAction=new QAction(icon,text,this);
    mAction->setText(text);
}

QAction *ReosFormAction::action() const {return mAction;}

ReosFormText::ReosFormText(ReosForm *parent, const QString &text):
    ReosFormObject(parent),mText(text)
{
    if (parent)
        parent->addForm(this);
}

void ReosFormText::setText(const QString &text)
{
    mText=text;
    emit textChanged(mText);
}
