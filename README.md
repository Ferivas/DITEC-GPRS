# DITEC-GPRS
DITEC basado en modem SIM908

# HARDWARE
Esta basado en el mainboard GARUA-PLV. Es necesario añadir un módulo RTC (DS3231 o DS1307). La conexión sugerida es en el conector de programación ISP.
Las conexiones son las siguientes:<br>

ISP        RTC<br>
1 GND     1 GND<br>
2 VCC     2 VCC<br>
3 NC      3 SDA<br>
4 NC      4 SCL<br>
5 SDA<br>
6 SCL<br>


## IGUALAR HORA CON SERVIDOR NTP

Es necesario hacer una conexión UDP a un servidor de la siguiente lista

https://tf.nist.gov/tf-cgi/servers.cgi

por el puerto 123. Luego de establecida la conexión se envía un buffer de 48 ceros (sin CR ni LF) y se procesa la respuesta.



