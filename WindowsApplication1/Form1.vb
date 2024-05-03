Public Class Form1
    Dim Kamera, SigNal, VerCB, Posleop, Sluch, Sluch2, Sluch3, Sluch4, Sluch5, Sluch6, Sluch7, Sluch8, Sluch9, Karma, Message, Message2, S, O, B, I, Kl, Fi, Ver, KofOg, Doh, Verp, Ranp, Ogn, Sog, SaveMoney, VerVal, VerTer, Men, Ranc, K, Hod As Integer

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Kamera = 0
        SigNal = 0
        Karma = 50
        Doh = 0
        K = 400000
        S = 3
        O = 0
        B = 0
        I = 2
        Kl = 10
        Fi = 1
        Ogn = 0
        Men = 1
        Hod = 0
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label45.Text = Men
        Label20.Text = Hod
        Label34.Text = ""
        Label38.Text = ""
        Label39.Text = ""
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        Button1.Text = "Рестарт"

    End Sub
    Function F(ByRef K, ByRef S, ByRef O, ByRef B, ByRef I, ByRef Kl, ByRef Fi, ByRef Men, ByRef Ogn)
        Button2.Enabled = False
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False
        Button6.Enabled = False
        Button7.Enabled = False
        Button8.Enabled = False
        Button9.Enabled = False
        Button10.Enabled = False
        Button11.Enabled = False
        Button12.Enabled = False
        Button13.Enabled = False
        Button14.Enabled = False
        Button1.Text = "Начать игру"
    End Function
    'начало ограблений
    Function Iog(ByVal I)
        If I >= 25 Then Vog(Ver, Ranc) Else K = K
    End Function
    Function Vog(ByVal Ver, Ranc)
        Ver = 20 + (I - 25) - B * 2
        If Ver < 15 Then Ver = 15 Else Ver = Ver
        If Ver > 75 Then Ver = 75 Else Ver = Ver
        Randomize()
        Ranc = CInt(100 * Rnd()) + 1
        If Ranc <= Ver Then Og(KofOg) Else K = K
    End Function
    Function Og(ByVal KofOg)
        KofOg = 20 + (I - 25) - O
        If KofOg > 65 Then KofOg = 65 Else KofOg = KofOg
        If KofOg < 10 Then KofOg = 10 Else KofOg = KofOg
        SaveMoney = O * 2
        Sog = (K / Fi / 100 * KofOg)
        If SaveMoney > 40 Then SaveMoney = 40 Else SaveMoney = SaveMoney
        Message = MessageBox.Show(("На наш филиал напали грабители и просят отдать выкуп за заложников. Отдать выкуп?"), ("Ограбление"), MessageBoxButtons.YesNo)
        If Message = DialogResult.Yes Then K = K - Sog + Sog / 100 * SaveMoney : I = I - I / 20 : Karma = Karma + 3 : MessageBox.Show((Sog / 100 * SaveMoney - Sog), ("Итоги ограбления"))
        If Message = DialogResult.No Then Kl = Kl - Kl / Fi / 4 : S = S - S / Fi / 5 : I = I - I / 20 : Karma = Karma - 5 : MessageBox.Show(("В ходе штурма погибло несколько клиентов и сотрудников, однако деньги не пострадали"), ("Итоги ограбления"))
    End Function
    'конец ограблений
    'начало пожара
    Function Pozh(ByVal Verp)
        Verp = 5
        Randomize()
        Ranp = CInt(100 * Rnd()) + 1
        If Ranp <= Verp Then Pozh2() Else K = K
    End Function
    Function Pozh2()
        If Ogn >= 1 Then Kl = Kl - Kl / Fi / 50 : Ogn = Ogn - 1 : K = K - 50000 : MessageBox.Show(("Пожар успешно нейтрализован, однако нам пришлось выплатить компенсацию потрадавшим в размере 50000р"), ("Итоги пожара")) Else K = K - 50000 - K / Fi / 10 : Kl = Kl - Kl / Fi / 10 : I = I - I / 50 : MessageBox.Show(("Ахтунг! Мы горим!"), ("Пожар")) : MessageBox.Show((-50000 - K / Fi / 10), ("Итоги пожара"))
    End Function
    'конец пожара
    'начало колебания валюты
    Function NaprVal(ByVal VerVal)
        Randomize()
        VerVal = CInt(100 * Rnd()) + 1
        If VerVal <= 50 Then Vslet(K, Kl) Else Padenie(K, Kl)
    End Function
    Function Vslet(ByRef K, ByRef Kl)
        K = K + K / 100 * 2
        Label38.Text = "Рубль поднялся"
        Label39.Text = K / 100 * 2
    End Function
    Function Padenie(ByRef K, ByRef Kl)
        K = K - K / 100 * 5
        Label38.Text = "Рубль опустился"
        Label39.Text = -K / 100 * 5
    End Function
    'конец колебаний валюты
    'Начало теракта
    Function Ter(ByRef Fi, ByRef K, ByRef S, ByRef I, ByRef B, ByRef O, ByRef Kl, ByVal VerTer)
        Randomize()
        VerTer = CInt(1000 * Rnd()) + 1
        If VerTer <= 7 Then K = K - K / Fi : Fi = Fi - 1 : I = I - I / 100 * 30 : B = B - B / Fi / 4 : Kl = Kl - Kl / Fi / 3 : S = S - S / Fi / 4 : O = O - O / Fi / 5 : MessageBox.Show(("В в результате теракта погибли треть клиентов находившихся в филиале, четверть сотрудников и пятая часть сотрудников. Деньги, находившиеся в хранилище, безвозвратно утерянны. Филиал разрушен и восстановлению не подлежит"), ("Экстренные новости")) Else K = K
        If B < 0 Then B = 0 Else B = B
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
    End Function
    'конец теракта
    'пожарная инспекция
    Function Pozhin(ByRef K)
        If Ogn < Fi * 4 Then K = K - K / 20 : MessageBox.Show(("Инспекция обнаружила серьезные нарушения в пожарной безопасности, поэтому с вас был взят штраф, составляющий 5% от капитала"), ("Итоги пожарной инспекции")) Else MessageBox.Show(("Инспеция не обнаружила нарушений в пожарной безопасности"), ("Итоги пожарной инспекции"))
    End Function
    'конец пожарной инспекции
    'проверка ЦБ
    Function CB(ByVal VerCB)
        Randomize()
        VerCB = CInt(10000 * Rnd()) + 1
        If VerCB = 5 Then MessageBox.Show(("Неожиданно нагрянула проверка из центрального банка. Они что-то кричали про план, а потом ушли. На следующий к вам пришло уведомление о том, что наш банк закрывается."), ("Итоги проверки из ЦБ")) : MessageBox.Show(("Попробуйте еще раз"), ("Игра окончена")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else MessageBox.Show(("Приходили люди из центрального банка, однако очень быстро ушли назад"), ("Итоги проверки ЦБ"))
    End Function
    'случайные события
    Function Sluchsob(ByVal Sluch)
        Randomize()
        Sluch = CInt(100 * Rnd()) + 1
        If Sluch >= 50 Then Sluchsob2(Sluch2) Else Sluchsob3(Sluch3)
    End Function
    Function Sluchsob2(ByVal Sluch2)
        Randomize()
        Sluch2 = CInt(100 * Rnd()) + 1
        If Sluch2 >= 50 Then Sluchsob4(Sluch4, K) Else K = K
    End Function
    Function Sluchsob3(ByVal Sluch3)
        Randomize()
        Sluch3 = CInt(100 * Rnd()) + 1
        If Sluch3 >= 50 Then Sluchsob5(Sluch5, S) Else K = K
    End Function
    Function Sluchsob4(ByVal Sluch4, ByRef K)
        Randomize()
        Sluch4 = CInt(100 * Rnd()) + 1
        If Sluch4 <= 50 Then Sluchsob7(Sluch7, K) Else K = K
    End Function
    Function Sluchsob5(ByVal Sluch5, ByRef S)
        Randomize()
        Sluch5 = CInt(100 * Rnd()) + 1
        If Sluch5 <= 50 Then Sluchsob8(Sluch8, S) Else Sluchsob6(Sluch6)
    End Function
    Function Sluchsob6(ByVal Sluch6)
        Randomize()
        Sluch6 = CInt(1000 * Rnd()) + 1
        If Sluch6 <= 2 Then MessageBox.Show(("На земле случился зомби-апокалипсис, и вы сбежали, спасая свою жизнь."), ("Экстренные новсти")) : MessageBox.Show(("Попробуйте еще раз"), ("Игра окончена")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Function
    Function Sluchsob7(ByVal Sluch7, ByRef K)
        Randomize()
        Sluch7 = CInt(100 * Rnd()) + 1
        If Sluch7 <= 30 Then MessageBox.Show(("На ваш филиал напала банда клоунов. Ваши охранники были настолько ошарашены, что не смогли их остановить."), ("Экстренные новости")) : K = K - K / Fi / 10 Else K = K
    End Function
    Function Sluchsob8(ByVal Sluch8, ByRef S)
        Randomize()
        Sluch7 = CInt(100 * Rnd()) + 1
        If Sluch7 <= 20 Then MessageBox.Show(("На ваш банк напали инопланетяне и похитили вашего сотрудника!"), ("Экстренные новости")) : Posleop = Hod + 5 : S = S - 1 Else K = K
    End Function
    Function Sluchsob8(ByVal Sluch9)
        Randomize()
        Sluch8 = CInt(100 * Rnd()) + 1
        If Sluch8 <= 2 Then Sobitie(Message2) Else K = K
    End Function
    Function Sobitie(ByVal Message2)
        Message2 = MessageBox.Show(("Куча школьниц хочет заручится вашей помощью в разгоне митинга 'Анти-Эпл'. Однако это будет стоить нам денег. Так вы согласны?"), (""))
        If Message2 = DialogResult.Yes Then Karma = Karma + 5 : K = K - K / 10
        If Message2 = DialogResult.No Then Karma = Karma - 7
    End Function
    'конец случайных событий
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'наем сотрудника
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 10000
        K = K + Doh
        S = S + 1
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        'повышение известности на 10
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 35000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 50) + 10 Else I = I + CInt(Kl / 150) + 10
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If S < 0 Then S = 0 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        'наем охранника
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 12000
        K = K + Doh
        S = S
        O = O + 1
        B = B + 2
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        'повышение безопасности на 10
        Kamera = Kamera + 1
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 50000
        K = K + Doh
        S = S
        O = O
        B = B + 10
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        'покупка филиала
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 200000
        K = K + Doh
        S = S
        O = O
        B = B - 10
        If I < 100 Then I = I + CInt(Kl / 50) + 15 Else I = I + CInt(Kl / 150) + 15
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi + 1
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        'повысить известность на 5
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 20000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 50) + 5 Else I = I + CInt(Kl / 150) + 5
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        'повысить известность на 2
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 10000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 50) + 2 Else I = I + CInt(Kl / 150) + 2
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        'повысить безопасность на 25
        Kamera = Kamera
        SigNal = SigNal + 1
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 70000
        K = K + Doh
        S = S
        O = O
        B = B + 25
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        'покупка огнетушителя
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 20000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn + 1
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click
        'покупка чужого банка
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 500000
        K = K + Doh
        S = S + 3
        O = O
        B = B - 10
        If I < 100 Then I = I + CInt(Kl / 70) + 20 Else I = I + CInt(Kl / 200) + 20
        If Kl < 200 Then Kl = Kl + CInt(I / 2) + 15 Else Kl = Kl + CInt(I / 5) + 15
        Fi = Fi + 1
        Ogn = Ogn
        Men = Men + 1
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If B < 0 Then B = 0 Else B = B
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        'ничего не делать
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click
        'нанять менеджера
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 15000
        K = K + Doh
        S = S
        O = O
        B = B
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men + 1
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        'уволить сотрудника
        Kamera = Kamera
        SigNal = SigNal
        Hod = Hod + 1
        Doh = Kl * 5000 - S * 25000 - O * 13000 - Fi * 10000 - Kl * 500 - Men * 30000 - 10000
        K = K + Doh
        S = S
        O = O - 1
        B = B
        If I < 100 Then I = I + CInt(Kl / 70) Else I = I + CInt(Kl / 200)
        If Kl < 200 Then Kl = Kl + CInt(I / 2) Else Kl = Kl + CInt(I / 5)
        Fi = Fi
        Ogn = Ogn
        Men = Men
        'терркат
        If Fi >= 2 Then Ter(Fi, K, VerTer, S, I, B, O, Kl) Else K = K
        'ограбление
        If K >= 150000 Then Iog(I) Else K = K
        'пожар
        If K >= 70000 Then Pozh(Verp) Else K = K
        'валюта
        If K >= 0 Then NaprVal(VerVal) Else NaprVal(VerVal)
        If (Hod + 1) Mod 24 = 0 Then MessageBox.Show(("Скоро приедет пожарная инспекция"), ("Важная информация")) Else K = K
        If Hod Mod 24 = 0 Then Pozhin(K) Else K = K
        If Hod >= 70 Then Sluchsob(Sluch) Else K = K
        If Men > Fi * 2 Then Men = Fi * 2 Else Men = Men
        If Posleop = Hod Then MessageBox.Show(("Инопланетяне вернули нашего сотрудника, однако он был настолько напуган, что ему пришлось выплатить компенсацию в размере 10000р за стресс на работе."), ("Экстренные новости")) : S = S + 1 : K = K - 10000
        If Hod Mod 75 = 0 Then CB(VerCB) Else K = K
        If S > Fi * 10 Then S = Fi * 10 Else S = S
        If S > Men * 5 Then S = Men * 5 Else S = S
        If Kl > S * 10 Then Kl = S * 10 Else Kl = Kl
        If B < 0 Then B = 0 Else B = B
        If Kl < 0 Then Kl = 0 Else Kl = Kl
        If I > Fi * 75 Then I = Fi * 75 Else I = I
        If I < 0 Then I = 0 Else I = I
        If S < 0 Then S = 0 Else S = S
        If O > Fi * 10 Then O = Fi * 10 Else O = O
        If SigNal > Fi Then SigNal = Fi Else SigNal = SigNal
        If Karma <= 0 Then MessageBox.Show(("Вы были слишком негуманны по отношению к другим, и поэтому БОГ решил вас покарать"), ("Гнев Божий")) : MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K
        Label2.Text = K
        Label4.Text = S
        Label6.Text = O
        Label9.Text = B
        Label10.Text = I
        Label12.Text = Kl
        Label14.Text = Fi
        Label26.Text = Ogn
        Label34.Text = Doh
        Label45.Text = Men
        Label20.Text = Hod
        If K < 15000 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 10000 Then Button2.Enabled = False Else Button2.Enabled = True
        If K < 35000 Then Button3.Enabled = False Else Button3.Enabled = True
        If K < 12000 Then Button4.Enabled = False Else Button4.Enabled = True
        If K < 50000 Then Button5.Enabled = False Else Button5.Enabled = True
        If K < 200000 Then Button6.Enabled = False Else Button6.Enabled = True
        If K < 20000 Then Button8.Enabled = False Else Button8.Enabled = True
        If K < 10000 Then Button9.Enabled = False Else Button9.Enabled = True
        If K < 70000 Then Button10.Enabled = False Else Button10.Enabled = True
        If K < 20000 Then Button11.Enabled = False Else Button11.Enabled = True
        If K < 500000 Then Button12.Enabled = False Else Button12.Enabled = True
        If S = Men * 5 Then Button2.Enabled = False Else Button2.Enabled = True
        If Men = Fi * 2 Then Button14.Enabled = False Else Button14.Enabled = True
        If K < 13000 Then Button7.Enabled = False Else Button7.Enabled = True
        If O <= 0 Then Button7.Enabled = False Else Button7.Enabled = True
        If O >= Fi * 10 Then Button4.Enabled = False Else Button4.Enabled = True
        If I >= Fi * 75 Then Button9.Enabled = False Else Button9.Enabled = True
        If I >= Fi * 75 Then Button8.Enabled = False Else Button8.Enabled = True
        If I >= Fi * 75 Then Button3.Enabled = False Else Button3.Enabled = True
        If SigNal >= Fi Then Button10.Enabled = False Else Button10.Enabled = True
        If Kamera >= Fi * 5 Then Button5.Enabled = False Else Button5.Enabled = True
        If K >= 0 Then Button13.Enabled = True Else Button13.Enabled = False
        If K <= 0 Then MessageBox.Show(("Ну сорян"), ("Вы проиграли")) : F(K, S, O, B, I, Kl, Fi, Men, Ogn) Else K = K

    End Sub



 

End Class
