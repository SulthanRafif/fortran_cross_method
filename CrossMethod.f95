program cross_method

  implicit none

  integer :: jumlah_sambungan, jumlah_batang, jumlah_siklus
  integer :: sambungan_ke, batang_ke, siklus_ke, perubahan_ke, setengah_perubahan_ke
  integer :: indeks_nilai_kekakuan, indeks_nilai_faktor_distribusi, indeks_nilai_fem, indeks_balance, indeks_co
  integer, allocatable, dimension(:) :: batang
  real, allocatable, dimension(:) :: nilai_kekakuan, faktor_distribusi, fem, fem_awal 
  real, allocatable, dimension(:) :: balance, list_co, momen_ujung_total, list_perubahan, list_setengah_perubahan, jumlah, theta
  integer :: balance_count_helper, co_count_helper, setengah_perubahan_count_helper
  integer :: negative_number
 
  real :: balance_a, balance_b, co, perubahan, setengah_perubahan
  integer :: indeks
  
  jumlah_sambungan = 3
  jumlah_siklus = 5 
  
  indeks_nilai_kekakuan = 1
  indeks_nilai_faktor_distribusi = 1
  indeks_nilai_fem = 1

  indeks_balance = 1
  indeks_co = 1
  
  allocate(batang(3))	

  batang(1) = 1
  batang(2) = 2
  batang(3) = 1

  jumlah_batang = batang(1) + batang(2) + batang(3)

  balance_count_helper = 1
  co_count_helper = 1
  setengah_perubahan_count_helper = 1
  negative_number = -1

  allocate(nilai_kekakuan(jumlah_batang))
  allocate(faktor_distribusi(jumlah_batang))
  allocate(fem(jumlah_batang))

  allocate(balance(jumlah_batang))
  allocate(list_co(jumlah_batang))
  allocate(momen_ujung_total(jumlah_batang))
  allocate(fem_awal(jumlah_batang))
  allocate(list_perubahan(jumlah_batang))
  allocate(list_setengah_perubahan(jumlah_batang))
  allocate(jumlah(jumlah_batang))
  allocate(theta(jumlah_batang))

  do sambungan_ke = 1, jumlah_sambungan
    print*,'Sambungan Ke - ', sambungan_ke
    do batang_ke = 1, batang(sambungan_ke)
      print*,'Batang Ke - ', batang_ke
    end do
  end do

  do sambungan_ke = 1, jumlah_sambungan
	print*,'Sambungan Ke - ', sambungan_ke
    do batang_ke = 1, batang(sambungan_ke)
		print*,'Masukkan Nilai Kekakuan Untuk Batang Ke - ', batang_ke
        read*, nilai_kekakuan(indeks_nilai_kekakuan)
        indeks_nilai_kekakuan = indeks_nilai_kekakuan + 1
    end do  
  end do

  print*,'Nilai Kekakuan untuk semua batang: ',nilai_kekakuan

  do sambungan_ke = 1, jumlah_sambungan
	print*,'Sambungan Ke - ', sambungan_ke
    do batang_ke = 1, batang(sambungan_ke)
		print*,'Masukkan Nilai Faktor Distribusi Untuk Batang Ke - ', batang_ke
        read*, faktor_distribusi(indeks_nilai_faktor_distribusi)
        indeks_nilai_faktor_distribusi = indeks_nilai_faktor_distribusi + 1
    end do  
  end do

  print*,'Nilai Faktor Distribusi untuk semua batang: ',faktor_distribusi

  do sambungan_ke = 1, jumlah_sambungan
	print*,'Sambungan Ke - ', sambungan_ke
    do batang_ke = 1, batang(sambungan_ke)
		print*,'Masukkan Nilai fem Untuk Batang Ke - ', batang_ke
        read*, fem(indeks_nilai_fem)

        fem_awal(indeks_nilai_fem) =  fem(indeks_nilai_fem) 
        momen_ujung_total(indeks_nilai_fem) = fem(indeks_nilai_fem)  
        
        indeks_nilai_fem = indeks_nilai_fem + 1
    end do  
  end do

  print*,'Nilai FEM untuk semua batang: ',fem

  do siklus_ke = 1, jumlah_siklus
    print*,'Siklus Ke - ', siklus_ke
    if (siklus_ke > 1)  then
        do sambungan_ke = 1, jumlah_sambungan
          print*,'Sambungan Ke - ', sambungan_ke
          do batang_ke = 1, batang(sambungan_ke)
            print*,'Perhitungan CO untuk batang ke - ', batang_ke
            
            indeks = indeks_co + co_count_helper
            list_co(indeks_co) = co(balance(indeks))
            co_count_helper = co_count_helper * negative_number

            momen_ujung_total(indeks_co) = momen_ujung_total(indeks_co) + list_co(indeks_co)
            
            indeks_co = indeks_co + 1
          end do  
        end do

        print*,'Nilai CO untuk siklus ke ', siklus_ke
        print*,list_co 

        do sambungan_ke = 1, jumlah_sambungan
          print*,'Sambungan Ke - ', sambungan_ke
          do batang_ke = 1, batang(sambungan_ke)
            print*,'Mengganti nilai FEM yang lama menjadi nilai FEM Baru Hasil Perhitungan CO untuk batang ke - ', batang_ke

            fem(indeks_balance) = list_co(indeks_balance)

            indeks_balance = indeks_balance + 1
          end do
        end do  

        indeks_balance = 1
        indeks_co = 1
    end if  
 
	do sambungan_ke = 1, jumlah_sambungan
    	print*,'Sambungan Ke - ', sambungan_ke
    	do batang_ke = 1, batang(sambungan_ke)
			if (batang(sambungan_ke) == 2) then
                print*,'Perhitungan Balance untuk batang ke - ', batang_ke
                
                indeks = indeks_balance + balance_count_helper
            	balance(indeks_balance) = balance_b(fem(indeks_balance), fem(indeks), faktor_distribusi(indeks_balance))
                balance_count_helper = balance_count_helper * negative_number

                momen_ujung_total(indeks_balance) = momen_ujung_total(indeks_balance) + balance(indeks_balance)
            else
				print*,'Perhitungan Balance untuk batang ke - ', batang_ke
            	balance(indeks_balance) = balance_a(fem(indeks_balance), faktor_distribusi(indeks_balance))

                momen_ujung_total(indeks_balance) = momen_ujung_total(indeks_balance) + balance(indeks_balance)
            end if
            indeks_balance = indeks_balance + 1
		end do
    end do
    indeks_balance = 1
    indeks_co = 1
    
    print*,'Nilai balance untuk siklus ke ', siklus_ke
    print*,balance 
  end do
  
  print*,'Momen Ujung Total ', momen_ujung_total
  print*,'Nilai FEM Awal ', fem_awal
  
  do perubahan_ke = 1, jumlah_batang
    list_perubahan(perubahan_ke) = perubahan(fem_awal(perubahan_ke), momen_ujung_total(perubahan_ke))
  end do  

  do setengah_perubahan_ke = 1, jumlah_batang
    indeks = setengah_perubahan_ke + setengah_perubahan_count_helper
    list_setengah_perubahan(setengah_perubahan_ke) = setengah_perubahan(list_perubahan(indeks))
    setengah_perubahan_count_helper = setengah_perubahan_count_helper * negative_number

    jumlah(setengah_perubahan_ke) = list_perubahan(setengah_perubahan_ke) + list_setengah_perubahan(setengah_perubahan_ke)
    theta(setengah_perubahan_ke) = (jumlah(setengah_perubahan_ke) / nilai_kekakuan(setengah_perubahan_ke)) * negative_number
  end do 
    
  print*,'Nilai Perubahan ', list_perubahan
  print*,'Nilai Setengah Perubahan ', list_setengah_perubahan
  print*,'Nilai Jumlah ', jumlah
  print*,'Nilai Theta ', theta

end program cross_method  

function balance_a(fem, faktor_distribusi)
  implicit none
  real :: fem,faktor_distribusi, balance_a
  integer :: minus
  minus = -1
  
  ! perhitungan balance a
  balance_a = (fem * faktor_distribusi) / minus

end function 

function balance_b(fem_a, fem_b, faktor_distribusi)
  implicit none
  real :: fem_a, fem_b, faktor_distribusi, balance_b
  integer :: minus
  minus = -1

  ! perhitungan balance b
  balance_b = (fem_a + fem_b) * faktor_distribusi / minus

end function

function co(balance)
  implicit none
  real :: co, balance 

  ! perhitungan CO
  co = balance / 2

end function

function perubahan(fem, momen_ujung_total)
  implicit none
    
  integer :: negative_number = -1
  real :: fem, momen_ujung_total, perubahan
  
  ! perhitungan perubahan
  perubahan = (fem - momen_ujung_total) / negative_number
  
end function 

function setengah_perubahan(perubahan)
  implicit none

  real :: perubahan, setengah_perubahan
  integer :: negative_number = -1

  ! perhitungan setengah perubahan
  setengah_perubahan = (perubahan / 2) * negative_number
  
end function

