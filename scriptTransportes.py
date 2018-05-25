import re

ciudades = []
transportes = []

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
        s += 'id: ' + str(self.id_) + '\n'
        s += 'continente: ' + self.continente_ + '\n'
        s += 'nombre: ' + self.nombre_ + '\n'
        s += 'pais: ' + self.pais_ + '\n'
        s += 'posX: ' + str(self.posX_) + '\n'
        s += 'posY: ' + str(self.posY_) +  '\n'
        return s

class Transporte:
    def __init__(self, id, calidad, ciudad_origen, ciudad_destino, precio, tipo):
        self.id_ = id
        self.calidad_ = calidad
        self.ciudad_origen_ = ciudad_origen
        self.ciudad_destino_ = ciudad_destino        
        self.precio_ = precio
        self.tipo_ = tipo

    def __str__(self):
        s = '################### INSTANCIA DE TRANSPORTE ###################\n'
        s += 'id: ' + str(self.id_) + '\n'
        s += 'calidad: ' + self.calidad_ + '\n'
        s += 'ciudad_origen: ' + find(self.ciudad_origen_).nombre_ + '\n'
        s += 'ciudad_destino: ' + find(self.ciudad_destino_).nombre_ + '\n'
        s += 'precio: ' + str(self.precio_) + '\n'
        s += 'tipo: ' + self.tipo_ +  '\n'
        return s

def find(id) :
    for x in ciudades:
        if x.id_ == id:
            return x

def encuentra_ciudades():
    pins = open('Practica.pins', 'r')

    pattern = '\(\[Practica_Class[0-9]*\]\s*of\s*Ciudad\n*\s*\(Continente\s*"\w*"\)\n\s*\(Nombre\s*"\w*"\)\n\s*\(Pais\s*"\w*"\)\n\s*\(PosX\s\W*\d*\W\d+\)\n\s*\(PosY\s\W*\d*\W\d+\)'
    content = pins.read()
    p = re.compile(pattern)
    found = p.findall(content)
    for i in found:
        id = int(re.search('Practica_Class[0-9]*', i).group().replace('Practica_Class',''))
        continente = re.search('Continente\s*"\w*"', i).group().replace('Continente ','').replace('"','')
        nombre = re.search('Nombre\s*"\w*"', i).group().replace('Nombre ','').replace('"','')
        pais = re.search('Pais\s*"\w*"', i).group().replace('Pais ', '').replace('"','')
        posX = float(re.search('PosX\s\W*\d*\W\d+', i).group().replace('PosX ', ''))
        posY = float(re.search('PosY\s\W*\d*\W\d+', i).group().replace('PosY ', ''))
        
        nueva_ciudad = Ciudad(id,continente,nombre,pais,posX,posY)
        ciudades.append(nueva_ciudad)

def encuentra_transportes():
    pins = open('Practica.pins', 'r')

    pattern = '\(\[Practica_Class[0-9]*\]\sof\s*Transporte\n*\s*\(Calidad\s\w*\)\n\s*\(CiudadDestino\s\[Practica_Class[0-9]*\]\)\n\s*\(CiudadOrigen\s\[Practica_Class[0-9]*\]\)\n\s*\(Precio\s\d*\)\n\s*\(Tipo\s\w*\)\)'
    content = pins.read()
    p = re.compile(pattern)
    found = p.findall(content)
    for i in found:
        id = int(re.search('Practica_Class[0-9]*', i).group().replace('Practica_Class',''))
        calidad = re.search('Calidad\s\w*', i).group().replace('Calidad ','')
        ciudad_origen = int(re.search('CiudadOrigen\s\[Practica_Class[0-9]*\]',i).group().replace('CiudadOrigen [Practica_Class', '').replace(']',''))
        ciudad_destino = int(re.search('CiudadDestino\s\[Practica_Class[0-9]*\]',i).group().replace('CiudadDestino [Practica_Class', '').replace(']',''))
        precio = int(re.search('Precio\s[0-9]*',i).group().replace('Precio ',''))
        tipo = re.search('Tipo\s\w*',i).group().replace('Tipo ','')

        nuevo_transporte = Transporte(id,calidad,ciudad_origen,ciudad_destino,precio,tipo)
        transportes.append(nuevo_transporte)

def ensena_ciudades():
    for i in ciudades:
        print(i)

def ensena_transportes():
    for i in transportes:
        print(i)

def main():
    encuentra_ciudades()
    #ensena_ciudades()
    encuentra_transportes()
    #ensena_transportes()

    print('Numero de ciudades: ' + str(len(ciudades)))
    print('Numero de transportes: ' + str(len(transportes)))


if __name__ == "__main__":
    main()