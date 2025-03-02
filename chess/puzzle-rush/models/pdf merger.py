from PyPDF2 import PdfMerger

# Create an instance of PdfMerger
merger = PdfMerger()

# List of PDF files to merge
pdf_files = ['/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/MGK_project.pdf',
             '/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/Academ_CV_MO.pdf',
             '/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/writing_sample_ Onoeva_Stankova__Czech_and_Russian_PQs.pdf',
             '/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/writing_sample_DAAD.pdf',
            '/Users/maria.onoeva/Downloads/diplom_bc_mo_comp.pdf',
'/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/diploma_mgr_eng.pdf',
'/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/diploma_supplement_MGR.pdf',
'/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/potvrz_stud_2024.pdf' ]

# Append each PDF file to the merger object
for pdf in pdf_files:
    merger.append(pdf)

# Write out the merged PDF to a file
merger.write('/Users/maria.onoeva/Desktop/AwesomeVault/BERLIN/Register CRC:SFB/MO_application_CRC_merged.pdf')
merger.close()
