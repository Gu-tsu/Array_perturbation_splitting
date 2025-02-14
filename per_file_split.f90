!------------------------------------------------------
MODULE common_vars
    IMPLICIT NONE
    INTEGER, PARAMETER :: nr = 129, nth = 8, nz = 321
    INTEGER, PARAMETER :: nr_h = 129, nth_h = 360, nz_h = 21
    INTEGER :: i, j, k, n
    REAL(KIND=8), DIMENSION(nr, nth, nz) :: read_file
    REAL(KIND=8), DIMENSION(nr, nz) :: read_file_2D
    REAL(KIND=8), DIMENSION(2 * nr - 1, nz) :: out_double_Horizontal, out_double_vertical
    REAL(KIND=8), DIMENSION(nr_h, nth_h, nz_h) :: angle_expansion_file
    CHARACTER(LEN=30) :: input_filename, output_0th, output_5th, output_3and7th, output_0and5th, output_4nz, output_reflection
END MODULE common_vars
!------------------------------------------------------
PROGRAM reshape_and_extract_all
    USE common_vars
    IMPLICIT NONE
 
    DO n = 113000, 113600, 200      ! 左边：从...开始、右边：步长为...、中间：直到...
!    DO n = 250400, 250400, 250400    ! 处理一个文件的方式
        PRINT*, n
        CALL construct_filenames()      ! 构造输入和输出文件名
!功能A（笛卡尔坐标）和功能B（圆形弧度坐标）只能二选一，第一个read...子函数必须开启
!--------------------------------------A  
!        CALL read_input_file()          ! 读取数据
!        CALL write_0th()                ! 输出0角度平面 (0)
!        CALL write_5th()                ! 输出5角度平面 (pi)
!        CALL extract_3and7th()          ! 输出3+7角度组合平面 (pi/4 + 3pi/4)
!        CALL extract_0and5th()          ! 输出0+5角度组合平面 (0 + pi)
!--------------------------------------B  
        CALL read_expansion_file()      ! 读取数据
        CALL write_50nz()               ! 输出角度扩展平面（50nz）
!-------------------------------------- 
!        CALL Remove_reflection()        ! 去掉反射抑制层
!-------------------------------------- 
    END DO
END PROGRAM reshape_and_extract_all
!------------------------------------------------------
SUBROUTINE construct_filenames()
    USE common_vars
    IMPLICIT NONE

    WRITE(input_filename, '(I6.6)') n
        input_filename = 'input/' // TRIM(ADJUSTL(input_filename)) // '.txt'
        output_0th = '0th/' // TRIM(input_filename(7:12)) // '.txt'
		output_5th = '5th/' // TRIM(input_filename(7:12)) // '.txt'
        output_3and7th = '3and7th/' // TRIM(input_filename(7:12)) // '.txt'
		output_0and5th = '0and5th/' // TRIM(input_filename(7:12)) // '.txt'
        output_4nz = '50nz/' // TRIM(input_filename(7:12)) // '.txt'
        output_reflection = 'unreflection/' // TRIM(input_filename(7:12)) // '.txt'
END SUBROUTINE construct_filenames
!------------------------------------------------------
SUBROUTINE read_input_file()
    USE common_vars
    IMPLICIT NONE

    OPEN(11, FILE=input_filename, STATUS='OLD')
    rewind(11)
    DO i = 1, nr
        DO j = 1, nth
           READ(11, '(321(1X,E22.15))') (read_file(i, j, k), k = 1, nz)
        END DO
    END DO
    CLOSE(11)
END SUBROUTINE read_input_file
!------------------------------------------------------
SUBROUTINE write_0th()
    USE common_vars
    IMPLICIT NONE

    OPEN(12, FILE=output_0th, STATUS='REPLACE')
    DO i = 1, nr
        WRITE(12, '(321(1X,E22.15))') (read_file(i, 1, k), k = 1, nz)
    END DO
    CLOSE(12)
END SUBROUTINE write_0th
!------------------------------------------------------
SUBROUTINE write_5th()
    USE common_vars
    IMPLICIT NONE

    OPEN(13, FILE=output_5th, STATUS='REPLACE')
    rewind(13)
    DO i = 1, nr
        WRITE(13, '(321(1X,E22.15))') (read_file(i, 5, k), k = 1, nz)
    END DO
    CLOSE(13)
END SUBROUTINE write_5th
!------------------------------------------------------
SUBROUTINE extract_3and7th()
    USE common_vars
    IMPLICIT NONE

    out_double_Horizontal = 0.0D0
    DO i = 1, nr
        DO j = 1, nth
            DO k = 1, nz
                IF (j .EQ. 1 + nth / 4) THEN
                    out_double_Horizontal(nr - 1 + i, k) = read_file(i, j, k)
                ELSE IF (j == 1 + 3 * nth / 4) THEN
                    out_double_Horizontal(nr + 1 - i, k) = read_file(i, j, k)
                END IF
            END DO
        END DO
    END DO

    OPEN(14, FILE=output_3and7th, STATUS='REPLACE')
    rewind(14)
    DO i = 1, 2 * nr - 1
        WRITE(14, '(321(1X,E22.15))') (out_double_Horizontal(i, k), k = 1, nz)
    END DO
    CLOSE(14)
END SUBROUTINE extract_3and7th
!------------------------------------------------------
SUBROUTINE extract_0and5th()
    USE common_vars
    IMPLICIT NONE

    out_double_vertical = 0.0D0
    DO i = 1, nr
        DO j = 1, nth
            DO k = 1, nz
                IF (j .EQ. 1) THEN
                    out_double_vertical(nr - 1 + i, k) = read_file(i, j, k)
                ELSE IF (j .EQ. 1 + nth / 2) THEN
                    out_double_vertical(nr + 1 - i, k) = read_file(i, j, k)
                END IF
            END DO
        END DO
    END DO

    OPEN(15, FILE=output_0and5th, STATUS='REPLACE')
    DO i = 1, 2 * nr - 1
        WRITE(15, '(321(1X,E22.15))') (out_double_vertical(i, k), k = 1, nz)
    END DO
    CLOSE(15)
END SUBROUTINE extract_0and5th
!------------------------------------------------------    
SUBROUTINE read_expansion_file
    USE common_vars
    IMPLICIT NONE
    
    OPEN(16, FILE=input_filename, STATUS='OLD')
    rewind(16)
    DO i = 1, nr_h
        DO j = 1, nth_h
            READ(16, '(21(1X,E22.15))') (angle_expansion_file(i, j, k), k = 1, nz_h)
        END DO
    END DO
    CLOSE(16)
END SUBROUTINE read_expansion_file
!------------------------------------------------------
SUBROUTINE write_50nz()
    USE common_vars
    IMPLICIT NONE
    
    OPEN(17, FILE=output_4nz, STATUS='REPLACE')
    DO i = 1, nr_h
        WRITE(17, '(360(1X,E22.15))') (angle_expansion_file(i, j, 4), j = 1, nth_h)
    END DO
    CLOSE(17)
END SUBROUTINE write_50nz
!------------------------------------------------------
SUBROUTINE Remove_reflection()
    USE common_vars
    IMPLICIT NONE

    OPEN(18, FILE=input_filename, STATUS='OLD')
    rewind(18)
    DO i = 1, nr
        !DO j = 1, nth
        !   READ(18, '(321(1X,E22.15))') (read_file(i, j, k), k = 1, nz)
        !END DO
        READ(18, '(321(1X,E22.15))') (read_file_2D(i, k), k = 1, nz)
    END DO
    CLOSE(18)

    !OPEN(19, FILE=output_reflection, STATUS='REPLACE')
    !DO i = 1, nr
    !    DO j = 1, nth
    !        DO k = 33, nz-32
    !            WRITE(19, '(1X,E22.15)', ADVANCE='NO') read_file(i, j, k)
    !        END DO
    !    END DO
    !    WRITE(19,'(A)') ''
    !END DO
    !CLOSE(19)
    
    OPEN(19, FILE=output_reflection, STATUS='REPLACE')
    DO i = 1, nr
        DO k = 33, nz-32
            WRITE(19, '(1X,E22.15)', ADVANCE='NO') read_file_2D(i, k)
        END DO
        WRITE(19,'(A)') ''
    END DO
END SUBROUTINE Remove_reflection    
!------------------------------------------------------
