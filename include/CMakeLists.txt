# FILE(GLOB INCLUDES_MK "*.mk") 
install(FILES GNU.mk IBM.mk Intel.mk NAG.mk PGI.mk extensions.mk driver.F90 DESTINATION include)
install(FILES base-install.mk DESTINATION include RENAME base.mk)


