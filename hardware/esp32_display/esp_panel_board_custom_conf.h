/**
 * Custom-Board-Config fuer Waveshare ESP32-S3-Touch-LCD-4 (480x480, ST7701 + GT911).
 * Basiert auf der offiziellen 2.1"-Config, angepasst mit den Werten unseres Boards
 * (Pins/Timings/ST7701-Init aus dem funktionierenden Waveshare-Beispiel 09_LVGL_Widgets).
 *
 * Diese Datei gehoert IN den Sketch-Ordner (neben smartrace_display.ino).
 */
#pragma once

// *INDENT-OFF*

// Diese Custom-Config verwenden (statt eines "supported board"):
#define ESP_PANEL_BOARD_USE_CUSTOM          (1)

#if ESP_PANEL_BOARD_USE_CUSTOM

#define ESP_PANEL_BOARD_NAME                "Custom:ESP32-S3-Touch-LCD-4"

#define ESP_PANEL_BOARD_WIDTH               (480)
#define ESP_PANEL_BOARD_HEIGHT              (480)

////////////////////////////////////////////// LCD //////////////////////////////////////////////
#define ESP_PANEL_BOARD_USE_LCD             (1)

#if ESP_PANEL_BOARD_USE_LCD
#define ESP_PANEL_BOARD_LCD_CONTROLLER      ST7701
#define ESP_PANEL_BOARD_LCD_BUS_TYPE        (ESP_PANEL_BUS_TYPE_RGB)

#if ESP_PANEL_BOARD_LCD_BUS_TYPE == ESP_PANEL_BUS_TYPE_RGB
    #define ESP_PANEL_BOARD_LCD_RGB_USE_CONTROL_PANEL       (1)

#if ESP_PANEL_BOARD_LCD_RGB_USE_CONTROL_PANEL
    /* 3-wire-SPI (nur fuer die ST7701-Init). CS ist bei diesem Board GPIO 42 (direkt). */
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_IO_CS               (42)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_IO_SCK              (2)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_IO_SDA              (1)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_CS_USE_EXPNADER     (0)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_SCL_USE_EXPNADER    (0)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_SDA_USE_EXPNADER    (0)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_MODE                (0)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_CMD_BYTES           (1)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_PARAM_BYTES         (1)
    #define ESP_PANEL_BOARD_LCD_RGB_SPI_USE_DC_BIT          (1)
#endif

    /* RGB-Refresh: Timings aus 09_LVGL_Widgets. PCLK bewusst niedrig gegen Underrun. */
    #define ESP_PANEL_BOARD_LCD_RGB_CLK_HZ          (14 * 1000 * 1000)
    #define ESP_PANEL_BOARD_LCD_RGB_HPW             (8)
    #define ESP_PANEL_BOARD_LCD_RGB_HBP             (50)
    #define ESP_PANEL_BOARD_LCD_RGB_HFP             (10)
    #define ESP_PANEL_BOARD_LCD_RGB_VPW             (8)
    #define ESP_PANEL_BOARD_LCD_RGB_VBP             (20)
    #define ESP_PANEL_BOARD_LCD_RGB_VFP             (10)
    #define ESP_PANEL_BOARD_LCD_RGB_PCLK_ACTIVE_NEG (0)
    #define ESP_PANEL_BOARD_LCD_RGB_DATA_WIDTH      (16)
    #define ESP_PANEL_BOARD_LCD_RGB_PIXEL_BITS      (ESP_PANEL_LCD_COLOR_BITS_RGB565)
    /* Bounce-Buffer gegen Screen-Drift/Underrun (WIDTH * 10, N=48 gerade). */
    #define ESP_PANEL_BOARD_LCD_RGB_BOUNCE_BUF_SIZE (ESP_PANEL_BOARD_WIDTH * 10)

    /* RGB-Pins (identisch zu 09_LVGL_Widgets). DATA0..4=B, 5..10=G, 11..15=R. */
    #define ESP_PANEL_BOARD_LCD_RGB_IO_HSYNC        (38)
    #define ESP_PANEL_BOARD_LCD_RGB_IO_VSYNC        (39)
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DE           (40)
    #define ESP_PANEL_BOARD_LCD_RGB_IO_PCLK         (41)
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DISP         (-1)
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA0        (5)     // B0
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA1        (45)    // B1
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA2        (48)    // B2
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA3        (47)    // B3
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA4        (21)    // B4
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA5        (14)    // G0
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA6        (13)    // G1
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA7        (12)    // G2
#if ESP_PANEL_BOARD_LCD_RGB_DATA_WIDTH > 8
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA8        (11)    // G3
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA9        (10)    // G4
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA10       (9)     // G5
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA11       (46)    // R0
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA12       (3)     // R1
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA13       (8)     // R2
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA14       (18)    // R3
    #define ESP_PANEL_BOARD_LCD_RGB_IO_DATA15       (17)    // R4
#endif
#endif // ESP_PANEL_BOARD_LCD_BUS_TYPE == RGB

#if (ESP_PANEL_BOARD_LCD_BUS_TYPE == ESP_PANEL_BUS_TYPE_RGB) && ESP_PANEL_BOARD_LCD_RGB_USE_CONTROL_PANEL
#define ESP_PANEL_BOARD_LCD_FLAGS_ENABLE_IO_MULTIPLEX       (0)
#define ESP_PANEL_BOARD_LCD_FLAGS_MIRROR_BY_CMD             (!ESP_PANEL_BOARD_LCD_FLAGS_ENABLE_IO_MULTIPLEX)
#endif

/* ST7701-Init unseres Panels (aus Arduino_GFX st7701_type1_init_operations). */
#define ESP_PANEL_BOARD_LCD_VENDOR_INIT_CMD() \
    { \
        {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x10}, 5, 0}, \
        {0xC0, (uint8_t []){0x3B, 0x00}, 2, 0}, \
        {0xC1, (uint8_t []){0x0D, 0x02}, 2, 0}, \
        {0xC2, (uint8_t []){0x31, 0x05}, 2, 0}, \
        {0xCD, (uint8_t []){0x08}, 1, 0}, \
        {0xB0, (uint8_t []){0x00, 0x11, 0x18, 0x0E, 0x11, 0x06, 0x07, 0x08, 0x07, 0x22, 0x04, 0x12, 0x0F, 0xAA, 0x31, \
                            0x18}, 16, 0}, \
        {0xB1, (uint8_t []){0x00, 0x11, 0x19, 0x0E, 0x12, 0x07, 0x08, 0x08, 0x08, 0x22, 0x04, 0x11, 0x11, 0xA9, 0x32, \
                            0x18}, 16, 0}, \
        {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x11}, 5, 0}, \
        {0xB0, (uint8_t []){0x60}, 1, 0}, \
        {0xB1, (uint8_t []){0x32}, 1, 0}, \
        {0xB2, (uint8_t []){0x07}, 1, 0}, \
        {0xB3, (uint8_t []){0x80}, 1, 0}, \
        {0xB5, (uint8_t []){0x49}, 1, 0}, \
        {0xB7, (uint8_t []){0x85}, 1, 0}, \
        {0xB8, (uint8_t []){0x21}, 1, 0}, \
        {0xC1, (uint8_t []){0x78}, 1, 0}, \
        {0xC2, (uint8_t []){0x78}, 1, 0}, \
        {0xE0, (uint8_t []){0x00, 0x1B, 0x02}, 3, 0}, \
        {0xE1, (uint8_t []){0x08, 0xA0, 0x00, 0x00, 0x07, 0xA0, 0x00, 0x00, 0x00, 0x44, 0x44}, 11, 0}, \
        {0xE2, (uint8_t []){0x11, 0x11, 0x44, 0x44, 0xED, 0xA0, 0x00, 0x00, 0xEC, 0xA0, 0x00, 0x00}, 12, 0}, \
        {0xE3, (uint8_t []){0x00, 0x00, 0x11, 0x11}, 4, 0}, \
        {0xE4, (uint8_t []){0x44, 0x44}, 2, 0}, \
        {0xE5, (uint8_t []){0x0A, 0xE9, 0xD8, 0xA0, 0x0C, 0xEB, 0xD8, 0xA0, 0x0E, 0xED, 0xD8, 0xA0, 0x10, 0xEF, 0xD8, \
                            0xA0}, 16, 0}, \
        {0xE6, (uint8_t []){0x00, 0x00, 0x11, 0x11}, 4, 0}, \
        {0xE7, (uint8_t []){0x44, 0x44}, 2, 0}, \
        {0xE8, (uint8_t []){0x09, 0xE8, 0xD8, 0xA0, 0x0B, 0xEA, 0xD8, 0xA0, 0x0D, 0xEC, 0xD8, 0xA0, 0x0F, 0xEE, 0xD8, \
                            0xA0}, 16, 0}, \
        {0xEB, (uint8_t []){0x02, 0x00, 0xE4, 0xE4, 0x88, 0x00, 0x40}, 7, 0}, \
        {0xEC, (uint8_t []){0x3C, 0x00}, 2, 0}, \
        {0xED, (uint8_t []){0xAB, 0x89, 0x76, 0x54, 0x02, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x20, 0x45, 0x67, 0x98, \
                            0xBA}, 16, 0}, \
        {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x13}, 5, 0}, \
        {0xE5, (uint8_t []){0xE4}, 1, 0}, \
        {0xFF, (uint8_t []){0x77, 0x01, 0x00, 0x00, 0x00}, 5, 0}, \
        {0x21, (uint8_t []){0x00}, 0, 0}, \
        {0x3A, (uint8_t []){0x60}, 1, 0}, \
        {0x11, (uint8_t []){0x00}, 0, 120}, \
        {0x29, (uint8_t []){0x00}, 0, 0}, \
    }

#define ESP_PANEL_BOARD_LCD_COLOR_BITS          (ESP_PANEL_LCD_COLOR_BITS_RGB565)
#define ESP_PANEL_BOARD_LCD_COLOR_BGR_ORDER     (0)
#define ESP_PANEL_BOARD_LCD_COLOR_INEVRT_BIT    (0)

// MIRROR_X + MIRROR_Y = 180 Grad -> entspricht der alten Arduino_GFX-Rotation 2.
// Falls das Bild kopfsteht: beide auf 0 setzen (bzw. eins von beiden).
#define ESP_PANEL_BOARD_LCD_SWAP_XY             (0)
#define ESP_PANEL_BOARD_LCD_MIRROR_X            (1)
#define ESP_PANEL_BOARD_LCD_MIRROR_Y            (1)
#define ESP_PANEL_BOARD_LCD_GAP_X               (0)
#define ESP_PANEL_BOARD_LCD_GAP_Y               (0)

/* Reset macht der CH32-IO-Expander (im Sketch vor board.init() angestossen). */
#define ESP_PANEL_BOARD_LCD_RST_IO              (-1)
#define ESP_PANEL_BOARD_LCD_RST_LEVEL           (0)

#endif // ESP_PANEL_BOARD_USE_LCD

///////////////////////////////////////////// TOUCH /////////////////////////////////////////////
// AUS: Der GT911 haengt am selben I2C-Bus wie der CH32-Expander. ESP32_Display_Panel
// wuerde dafuer den alten esp_lcd-I2C-Treiber nutzen, Arduino-Wire (CH32) den neuen
// (driver_ng) -> Abbruch "driver_ng is not allowed to be used with this old driver".
// Deshalb macht ESP_PANEL nur das Display; den GT911 treibt der Sketch selbst ueber
// Arduino-Wire (SensorLib) und haengt ihn als LVGL-Eingabegeraet ein.
#define ESP_PANEL_BOARD_USE_TOUCH               (0)

#if ESP_PANEL_BOARD_USE_TOUCH
#define ESP_PANEL_BOARD_TOUCH_CONTROLLER        GT911
#define ESP_PANEL_BOARD_TOUCH_BUS_TYPE          (ESP_PANEL_BUS_TYPE_I2C)

#if (ESP_PANEL_BOARD_TOUCH_BUS_TYPE == ESP_PANEL_BUS_TYPE_I2C)
    #define ESP_PANEL_BOARD_TOUCH_BUS_SKIP_INIT_HOST        (0)
    #define ESP_PANEL_BOARD_TOUCH_I2C_HOST_ID               (0)
    #define ESP_PANEL_BOARD_TOUCH_I2C_CLK_HZ                (400 * 1000)
    #define ESP_PANEL_BOARD_TOUCH_I2C_SCL_PULLUP            (1)
    #define ESP_PANEL_BOARD_TOUCH_I2C_SDA_PULLUP            (1)
    #define ESP_PANEL_BOARD_TOUCH_I2C_IO_SCL                (7)
    #define ESP_PANEL_BOARD_TOUCH_I2C_IO_SDA                (15)
    #define ESP_PANEL_BOARD_TOUCH_I2C_ADDRESS               (0)     // 0 = Default (GT911: 0x5D)
#endif

#define ESP_PANEL_BOARD_TOUCH_SWAP_XY           (0)
#define ESP_PANEL_BOARD_TOUCH_MIRROR_X          (0)
#define ESP_PANEL_BOARD_TOUCH_MIRROR_Y          (0)
#define ESP_PANEL_BOARD_TOUCH_RST_IO            (-1)
#define ESP_PANEL_BOARD_TOUCH_RST_LEVEL         (0)
#define ESP_PANEL_BOARD_TOUCH_INT_IO            (-1)
#define ESP_PANEL_BOARD_TOUCH_INT_LEVEL         (0)
#endif // ESP_PANEL_BOARD_USE_TOUCH

/* Backlight + IO-Expander uebernimmt WS_CH32_IO im Sketch (board_init) -> hier AUS. */
#define ESP_PANEL_BOARD_USE_BACKLIGHT           (0)
#define ESP_PANEL_BOARD_USE_EXPANDER            (0)

#define ESP_PANEL_BOARD_CUSTOM_FILE_VERSION_MAJOR 1
#define ESP_PANEL_BOARD_CUSTOM_FILE_VERSION_MINOR 0
#define ESP_PANEL_BOARD_CUSTOM_FILE_VERSION_PATCH 0

#endif // ESP_PANEL_BOARD_USE_CUSTOM

// *INDENT-ON*
