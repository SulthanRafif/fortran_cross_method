program cross_method

implicit none

integer :: jumlah_sambungan
integer :: sambungan_ke
integer,allocatable,dimension(:) :: batang

integer :: batang_ke

print*,'Masukkan jumlah sambungan:'
read*, jumlah_sambungan

print*,'jumlah sambungan yang dimasukkan ',jumlah_sambungan

allocate(batang(jumlah_sambungan))

do sambungan_ke=0,jumlah_sambungan-1
  print*,'Masukkan jumlah batang untuk sambungan ke ',sambungan_ke+1
  read*,batang(sambungan_ke+1)
end do

do sambungan_ke=0, jumlah_sambungan-1
  print*,'Memasukkan nilai kekakuan, faktor distribusi, dan FEM untuk sambungan ke ',sambungan_ke+1
  do batang_ke=0, batang(sambungan_ke+1)-1
	print*,'Batang Ke - ', batang_ke+1 
  end do
  batang_ke=0
end do

!print *, batang

deallocate(batang)

end program cross_method
