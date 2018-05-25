import re

class Ciudad:
    def __init__(self, id, continente, nombre, pais, posX, posY):
        self.id_ = id
        self.continente_ = continente
        self.nombre_ = nombre
        self.pais_ = pais
        self.posX_ = posX
        self.posY_ = posY

    def __str__(self):
        s = '################### INSTANCIA DE CIUDAD ###################\n'
        s +=  'id: ' + str(self.id_) + '\n'
        s += 'continente: ' + self.continente_ + '\n'
        s += 'nombre: ' + self.nombre_ + '\n'
        s += 'pais: ' + self.pais_ + '\n'
        s += 'posX: ' + str(self.posX_) + '\n'
        s += 'posY: ' + str(self.posY_) +  '\n'
        return s
        
ciudades = []

def encuentra_ciudades():
    pins = open('Practica.pins')

    pattern = '\(\[Practica_Class[0-9]*\]\s*of\s*Ciudad\n*\s*\(Continente\s*"\w*"\)\n\s*\(Nombre\s*"\w*"\)\n\s*\(Pais\s*"\w*"\)\n\s*\(PosX\s\W*\d*\W\d+\)\n\s*\(PosY\s\W*\d*\W\d+\)'
    content = pins.read()
    p = re.compile(pattern)
    found = p.findall(content)
    for i in range (0, len(found)):
        id = int(re.search('Practica_Class[0-9]*', found[i]).group().replace('Practica_Class',''))
        continente = re.search('Continente\s*"\w*"', found[i]).group().replace('Continente ','').replace('"','')
        nombre = re.search('Nombre\s*"\w*"', found[i]).group().replace('Nombre ','').replace('"','')
        pais = re.search('Pais\s*"\w*"', found[i]).group().replace('Pais ', '').replace('"','')
        posX = float(re.search('PosX\s\W*\d*\W\d+', found[i]).group().replace('PosX ', ''))
        posY = float(re.search('PosY\s\W*\d*\W\d+', found[i]).group().replace('PosY ', ''))
        nueva_ciudad = Ciudad(id,continente,nombre,pais,posX,posY)
        ciudades.append(nueva_ciudad)

def ensena_ciudades():
    for i in ciudades:
        print(i)

def main():
    encuentra_ciudades()
    ensena_ciudades()

if __name__ == "__main__":
    main()