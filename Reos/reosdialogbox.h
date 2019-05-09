#ifndef REOSDIALOGBOX_H
#define REOSDIALOGBOX_H

#include <QDialog>
#include <QDialogButtonBox>
#include <QVBoxLayout>


class ReosDialogBox:public QDialog
{
public:
    ReosDialogBox(QWidget* w,QWidget *parent=nullptr);

private:

    QVBoxLayout *mLayout;

};

#endif // REOSDIALOGBOX_H
