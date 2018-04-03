package com.ml.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.io.Serializable;
import java.util.Objects;

@Document(collection = "metric")
public class Metric implements Serializable {

    @Id
    private String id;

    @Field
    @Indexed
    private String codigo;

    @Field
    @Indexed
    private String estado;

    @Field
    private String descripcion;

    @Field
    @Indexed
    private String dia;

    @Field
    @Indexed
    private String linealidad;

    public Metric(){ }


    @Override
    public String toString() {
        return "Metric{" +
                "id='" + id + '\'' +
                ", codigo='" + codigo + '\'' +
                ", estado='" + estado + '\'' +
                ", descripcion='" + descripcion + '\'' +
                ", dia=" + dia +
                ", linealidad='" + linealidad + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Metric metric = (Metric) o;
        return Objects.equals(id, metric.id) &&
                Objects.equals(codigo, metric.codigo) &&
                Objects.equals(estado, metric.estado) &&
                Objects.equals(descripcion, metric.descripcion) &&
                Objects.equals(dia, metric.dia) &&
                Objects.equals(linealidad, metric.linealidad);
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, codigo, estado, descripcion, dia, linealidad);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getEstado() {
        return estado;
    }

    public void setEstado(String estado) {
        this.estado = estado;
    }

    public String getDescripcion() {
        return descripcion;
    }

    public void setDescripcion(String descripcion) {
        this.descripcion = descripcion;
    }

    public String getDia() {
        return dia;
    }

    public void setDia(String dia) {
        this.dia = dia;
    }

    public String getLinealidad() {
        return linealidad;
    }

    public void setLinealidad(String linealidad) {
        this.linealidad = linealidad;
    }
}
